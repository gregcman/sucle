/*
 * -----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <webmaster@flippeh.de> wrote this file. As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return. Lukas Niederbremer.
 * -----------------------------------------------------------------------------
 */

#include "nbt.h"

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

void dump_nbt(const char *filename);

int main(int argc, char **argv)
{
    int c;

    //opterr = 0;
    for (;;)
    {
        static struct option long_options[] =
        {
            {"version", no_argument, NULL, 'v'},
            {NULL,      no_argument, NULL, 0}
        };

        int option_index = 0;

        if ((c = getopt_long(argc, argv, "v", long_options, &option_index)) < 0)
            break;

        switch (c)
        {
            case 0:
                if (long_options[option_index].flag != 0)
                    break;

                break;

            case 'v':
                printf("%s 1.2 (%s, %s)\n", argv[0], __DATE__, __TIME__);

                return EXIT_SUCCESS;

            case '?':
                break;
        }
    }

    if (optind < argc)
    {
        /* Make sure a file was given */
        dump_nbt(argv[optind]);
    }

    return 0;
}

void dump_nbt(const char *filename)
{
    assert(errno == NBT_OK);

    FILE* f = fopen(filename, "rb");
    nbt_node* root = nbt_parse_file(f);
    fclose(f);

    if(errno != NBT_OK)
    {
        fprintf(stderr, "Parsing error!\n");
        return;
    }

    char* str = nbt_dump_ascii(root);
    nbt_free(root);

    if(str == NULL)
        fprintf(stderr, "Printing error!");

    printf("%s", str);

    free(str);
}
