#include "nbt.h"

#include <errno.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void die(const char* message)
{
    fprintf(stderr, "%s\n", message);
    exit(1);
}

static void die_with_err(int err)
{
    fprintf(stderr, "Error %i: %s\n", err, nbt_error_to_string(err));
    exit(1);
}

static nbt_node* get_tree(const char* filename)
{
    FILE* fp = fopen(filename, "rb");
    if(fp == NULL) die("Could not open the file for reading.");

    nbt_node* ret = nbt_parse_file(fp);
    if(ret == NULL) die_with_err(errno);
    fclose(fp);

    return ret;
}

static bool check_size(nbt_node* n, void* aux)
{
    (void)n;
    int* size = aux;
    *size += 1;

    return true;
}

int main(int argc, char** argv)
{
    if(argc == 1 || strcmp(argv[1], "--help") == 0)
    {
        printf("Usage: %s [nbt file]\n", argv[0]);
        return 0;
    }

    printf("Getting tree from %s... ", argv[1]);
    nbt_node* tree = get_tree(argv[1]);
    printf("OK.\n");

    /* Use this to refer to the tree in gdb. */
    char* the_tree = nbt_dump_ascii(tree);

    if(the_tree == NULL)
        die_with_err(errno);

    {
        printf("Checking nbt_map and nbt_size...");
        size_t mapped_size = 0;
        bool ret = nbt_map(tree, check_size, &mapped_size);
        size_t actual_size = nbt_size(tree);
        if(!ret)
            die("FAILED. nbt_map was terminated by a visitor, even though the visitor wants to do no such thing.");
        if(mapped_size != actual_size)
            die("FAILED. nbt_map and nbt_size are not playing nice.");
        printf("OK.\n");
    }

    {
        printf("Checking nbt_clone... ");
        nbt_node* clone = nbt_clone(tree);
        if(!nbt_eq(tree, clone))
            die("FAILED. Clones not equal.");
        nbt_free(tree); /* swap the tree out for its clone */
        tree = clone;
        printf("OK.\n");
    }

    FILE* temp = fopen("delete_me.nbt", "wb");
    if(temp == NULL) die("Could not open a temporary file.");

    nbt_status err;

    printf("Dumping binary... ");
    if((err = nbt_dump_file(tree, temp, STRAT_GZIP)) != NBT_OK)
        die_with_err(err);
    printf("OK.\n");

    fclose(temp);
    temp = fopen("delete_me.nbt", "rb");
    if(temp == NULL) die("Could not re-open a temporary file.");

    printf("Reparsing... ");
    nbt_node* tree_copy = nbt_parse_file(temp);
    if(tree_copy == NULL) die_with_err(errno);
    printf("OK.\n");

    printf("Checking trees... ");
    if(!nbt_eq(tree, tree_copy))
    {
        printf("Original tree:\n%s\n", the_tree);

        char* copy = nbt_dump_ascii(tree_copy);
        if(copy == NULL) die_with_err(err);

        printf("Reparsed tree:\n%s\n", copy);
        die("Trees not equal.");
    }
    printf("OK.\n");

    printf("Freeing resources... ");

    fclose(temp);

    if(remove("delete_me.nbt") == -1)
        die("Could not delete delete_me.nbt. Race condition?");

    nbt_free(tree);
    nbt_free(tree_copy);

    printf("OK.\n");

    free(the_tree);
    return 0;
}
