/*
 * @@BASEFILENAME@@
 *
 * Description goes here...
 * Author:  @@AUTHOR@@
 * Created:  @@DATE@@
 *
 * Last updated: <2013-March-19 12:39:36>
 * ------------------------------------------------------------------
 *
 * Copyright (c) Dino Chiesa 2013.  All rights reserved.
 *
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>



void Usage(char *appname)
{
    printf("\n@@BASEFILENAMELESSEXTENSION@@.exe\n");
    printf("  do something...\n\n");
    printf("usage:\n");
    printf("  %s -x <option>\n\n", appname);
    printf("  options:\n");
    printf("   -x <option>\n");
}


int main(int argc, char **argv)
{
    @@DOT@@

    if (argc==3)
    {
        if (_strnicmp(argv[1], DIR_SWITCH, strlen(DIR_SWITCH))==0) {
            strncpy_s(IniDir, MAX_PATH, argv[2], _MAX_PATH-1);
            sprintf_s(FullpathUrls, _MAX_PATH,"%s\\%s", argv[2], TEST_URLS);
        }
        else {
            Usage(argv[0]);
            exit(1);
        }
    }
    /*     else if (argc==1){ */
    /*          strcpy_s(IniDir, _MAX_PATH, ".");  */
    /*  strcpy_s(FullpathUrls, _MAX_PATH, TEST_URLS); */
    /*     } */
    else {
        Usage(argv[0]);
        exit(1);
    }

}
