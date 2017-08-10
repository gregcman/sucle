#include <stdio.h>
#include <stdlib.h>
 
#include <libavutil/opt.h>
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libswresample/swresample.h>

int find_first_audio_stream(AVFormatContext* format){
  // Find the index of the first audio stream
  int stream_index =- 1;
  for (int i=0; i<format->nb_streams; i++) {
    if (format->streams[i]->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
      stream_index = i;
      break;
    }
  }

  return stream_index;
}

void prepare_resampler (struct SwrContext* swr, AVCodecContext* codec, const int sample_rate){
  av_opt_set_int(swr, "in_channel_count",  codec->channels, 0);
  av_opt_set_int(swr, "out_channel_count", 1, 0);
  av_opt_set_int(swr, "in_channel_layout",  codec->channel_layout, 0);
  av_opt_set_int(swr, "out_channel_layout", AV_CH_LAYOUT_MONO, 0);
  av_opt_set_int(swr, "in_sample_rate", codec->sample_rate, 0);
  av_opt_set_int(swr, "out_sample_rate", sample_rate, 0);
  av_opt_set_sample_fmt(swr, "in_sample_fmt",  codec->sample_fmt, 0);
  av_opt_set_sample_fmt(swr, "out_sample_fmt", AV_SAMPLE_FMT_S16,  0);
  swr_init(swr);
}

int iterate_through_frames(AVFormatContext* format,
			    AVPacket* packet,
			    AVCodecContext* codec,
			    AVFrame* frame,
			    struct SwrContext* swr,
			    int16_t** data,
			    int* size){
  // iterate through frames
  *data = NULL;
  *size = 0;
  while (av_read_frame(format, packet) >= 0) {
    // decode one frame
    int gotFrame;
    if (avcodec_decode_audio4(codec, frame, &gotFrame, packet) < 0) {
      break;
    }
    if (!gotFrame) {
      continue;
    }
    // resample frames
    int16_t* buffer;
    av_samples_alloc((uint8_t**) &buffer, NULL, 1, frame->nb_samples, AV_SAMPLE_FMT_S16, 0);
    int frame_count = swr_convert(swr, (uint8_t**) &buffer, frame->nb_samples, (const uint8_t**) frame->data, frame->nb_samples);
    // append resampled frames to data
    *data = (int16_t*) realloc(*data, (*size + frame->nb_samples) * sizeof(int16_t));
    memcpy(*data + *size, buffer, frame_count * sizeof(int16_t));
    *size += frame_count;
  }
  return 123123;
}
 
int decode_audio_file(const char* path,
		      const int sample_rate,
		      int16_t** data,
		      int* size) {
 
  // initialize all muxers, demuxers and protocols for libavformat
  // (does nothing if called twice during the course of one program execution)
  av_register_all();
 
  // get format from audio file
  AVFormatContext* format = avformat_alloc_context();
  if (avformat_open_input(&format, path, NULL, NULL) != 0) {
    ///could not open file
    return -1;
  }
  if (avformat_find_stream_info(format, NULL) < 0) {
    ///could not retrieve stream info from file
    return -1;
  }

  int stream_index = find_first_audio_stream(format);
  if (stream_index == -1) {
    /// could not retrieve audio stream from file
    return -1;
  }
  AVStream* stream = format->streams[stream_index];
 
  // find & open codec
  AVCodecContext* codec = stream->codec;
  if (avcodec_open2(codec, avcodec_find_decoder(codec->codec_id), NULL) < 0) {
    ///failed to open decoder for stream number stream_index for file path
    return -1;
  }
 
  // prepare resampler
  struct SwrContext* swr = swr_alloc();
  prepare_resampler(swr, codec, sample_rate);
  if (!swr_is_initialized(swr)) {
    ///resampler not properly initialized
    return -1;
  }
 
  // prepare to read data
  AVPacket packet;
  av_init_packet(&packet);
  AVFrame* frame = av_frame_alloc();
  if (!frame) {
    ///error allocating the frame
    return -1;
  }
 
  iterate_through_frames(format, &packet, codec, frame, swr, data, size);
 
  // clean up
  av_frame_free(&frame);
  swr_free(&swr);
  avcodec_close(codec);
  avformat_free_context(format);
 
  // success
  return 26;
 
} 

/*int main(int argc, char const *argv[]) {
 
// check parameters
if (argc < 2) {
        fprintf(stderr, "Please provide the path to an audio file as first command-line argument.\n");
        return -1;
    }
 
    // decode data
    int sample_rate = 44100;
    double* data;
    int size;
    if (decode_audio_file(argv[1], sample_rate, &data, &size) != 0) {
        return -1;
    }
 
    // sum data
    double sum = 0.0;
    for (int i=0; i<size; ++i) {
        sum += data[i];
    }
 
    // display result and exit cleanly
    printf("sum is %f", sum);
    free(data);
    return 0;
    }*/
