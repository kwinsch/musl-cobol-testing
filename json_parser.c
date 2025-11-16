/* Simple JSON parser for COBOL - proof of concept */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* Simple struct to hold parsed customer data */
typedef struct {
    char name[50];
    int age;
    double balance;
    char status[20];
} Customer;

static Customer customers[100];
static int customer_count = 0;

/* Very simple JSON parser - extracts key-value pairs from object */
static void parse_customer(const char *json) {
    Customer *c = &customers[customer_count];

    /* Initialize */
    memset(c, 0, sizeof(Customer));

    /* Look for "name": "value" */
    const char *p = strstr(json, "\"name\"");
    if (p) {
        p = strchr(p, ':');
        if (p) {
            p = strchr(p, '"');
            if (p) {
                p++;
                const char *end = strchr(p, '"');
                if (end) {
                    int len = end - p;
                    if (len > 49) len = 49;
                    strncpy(c->name, p, len);
                }
            }
        }
    }

    /* Look for "age": number */
    p = strstr(json, "\"age\"");
    if (p) {
        p = strchr(p, ':');
        if (p) {
            c->age = atoi(p + 1);
        }
    }

    /* Look for "balance": number */
    p = strstr(json, "\"balance\"");
    if (p) {
        p = strchr(p, ':');
        if (p) {
            c->balance = atof(p + 1);
        }
    }

    /* Look for "status": "value" */
    p = strstr(json, "\"status\"");
    if (p) {
        p = strchr(p, ':');
        if (p) {
            p = strchr(p, '"');
            if (p) {
                p++;
                const char *end = strchr(p, '"');
                if (end) {
                    int len = end - p;
                    if (len > 19) len = 19;
                    strncpy(c->status, p, len);
                }
            }
        }
    }

    customer_count++;
}

/* Load JSON file containing array of customer objects */
int json_load_file(const char *filename_cobol) {
    /* Convert COBOL string to C string */
    char filename[256];
    int fn_len = 0;
    while (fn_len < 255 && fn_len < 50 && filename_cobol[fn_len] != ' ' && filename_cobol[fn_len] != '\0') {
        filename[fn_len] = filename_cobol[fn_len];
        fn_len++;
    }
    filename[fn_len] = '\0';

    FILE *f = fopen(filename, "r");
    if (!f) return -1;

    customer_count = 0;

    /* Read file line by line looking for objects */
    char line[512];
    char object[1024] = "";
    int in_object = 0;

    while (fgets(line, sizeof(line), f)) {
        /* Simple state machine - look for { } pairs */
        for (int i = 0; line[i]; i++) {
            if (line[i] == '{') {
                in_object = 1;
                object[0] = '\0';
            }

            if (in_object) {
                int len = strlen(object);
                if (len < sizeof(object) - 2) {
                    object[len] = line[i];
                    object[len + 1] = '\0';
                }
            }

            if (line[i] == '}' && in_object) {
                in_object = 0;
                parse_customer(object);
                if (customer_count >= 100) break;
            }
        }
    }

    fclose(f);
    return customer_count;
}

/* Get customer count - returns as int for now, COBOL will handle conversion */
int json_get_count(void) {
    return customer_count;
}

/* Get customer data by index (0-based) */
int json_get_customer(int index, char *name, char *status, char *age, char *balance) {
    if (index < 0 || index >= customer_count) return -1;

    Customer *c = &customers[index];

    /* Copy to COBOL strings (space-padded) */
    int i;
    for (i = 0; i < 50 && c->name[i]; i++) {
        name[i] = c->name[i];
    }
    for (; i < 50; i++) {
        name[i] = ' ';
    }

    for (i = 0; i < 20 && c->status[i]; i++) {
        status[i] = c->status[i];
    }
    for (; i < 20; i++) {
        status[i] = ' ';
    }

    /* Convert age to COBOL PIC 9(4) - 4 digit string */
    snprintf(age, 5, "%04d", c->age);

    /* Convert balance to COBOL PIC 9(8)V99 - 10 digit string with implied decimal */
    int balance_int = (int)(c->balance * 100);  /* Convert to cents */
    snprintf(balance, 11, "%010d", balance_int);

    return 0;
}
