/* C file I/O helper functions for COBOL */
#include <stdio.h>
#include <string.h>

/* Write a line to a file */
int c_write_line(const char *filename_cobol, const char *data, int data_len) {
    /* Convert COBOL string (space-padded) to C string (null-terminated) */
    char filename[256];
    int fn_len = 0;
    while (fn_len < 255 && fn_len < 20 && filename_cobol[fn_len] != ' ' && filename_cobol[fn_len] != '\0') {
        filename[fn_len] = filename_cobol[fn_len];
        fn_len++;
    }
    filename[fn_len] = '\0';

    FILE *f = fopen(filename, "a");
    if (!f) return -1;

    /* Remove trailing spaces from COBOL string */
    int len = data_len;
    while (len > 0 && data[len-1] == ' ') len--;

    fwrite(data, 1, len, f);
    fputc('\n', f);
    fclose(f);
    return 0;
}

/* Read a line from a file */
int c_read_line(const char *filename_cobol, char *buffer, int buffer_size) {
    static FILE *f = NULL;
    static char current_file[256] = "";

    /* Convert COBOL string to C string */
    char filename[256];
    int fn_len = 0;
    while (fn_len < 255 && fn_len < 20 && filename_cobol[fn_len] != ' ' && filename_cobol[fn_len] != '\0') {
        filename[fn_len] = filename_cobol[fn_len];
        fn_len++;
    }
    filename[fn_len] = '\0';

    /* Open on first call or if filename changed */
    if (f == NULL || strcmp(filename, current_file) != 0) {
        if (f != NULL) fclose(f);
        f = fopen(filename, "r");
        if (!f) return -1;
        strncpy(current_file, filename, sizeof(current_file) - 1);
    }

    /* Read line */
    if (fgets(buffer, buffer_size, f) == NULL) {
        fclose(f);
        f = NULL;
        current_file[0] = '\0';
        return -1;  /* EOF or error */
    }

    /* Remove newline and pad with spaces for COBOL */
    int len = strlen(buffer);
    if (len > 0 && buffer[len-1] == '\n') {
        buffer[len-1] = ' ';
    }

    /* Pad rest with spaces */
    for (int i = len; i < buffer_size; i++) {
        buffer[i] = ' ';
    }

    return 0;
}

/* Close and reset file handle */
void c_close_file(void) {
    /* Could maintain multiple handles in a real implementation */
}
