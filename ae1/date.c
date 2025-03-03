#include "date.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

struct date
{
    int day, month, year;
};

Date *date_create(char *datestr)
{

    if (datestr[2] != '/' || datestr[5] != '/' || strlen(datestr) != 10)
    {
        return NULL;
    }

    for (int i = 0; i < 10; i++)
    {
        if (i != 2 && i != 5 && isdigit(datestr[i]) == 0)
        {
            return NULL;
        }
    }

    Date *new_date = malloc(sizeof *new_date);
    if (!new_date)
    {
        return NULL;
    }
    new_date->day = atoi(strsep(&datestr, "/"));
    new_date->month = atoi(strsep(&datestr, "/"));
    new_date->year = atoi(datestr);

    if (new_date->day < 1 || new_date->month < 1 || new_date->year < 1)
    {
        free(new_date);
        return NULL;
    }

    if (new_date->day > 31 || new_date->month > 12)
    {
        free(new_date);
        return NULL;
    }

    return new_date;
}

Date *date_duplicate(Date *d)
{
    Date *date_copy = malloc(sizeof *date_copy);
    if (!date_copy)
    {
        return NULL;
    }
    date_copy->day = d->day;
    date_copy->month = d->month;
    date_copy->year = d->year;
    return date_copy;
}

int date_compare(Date *date1, Date *date2)
{
    int day1 = date1->day;
    int day2 = date2->day;
    int month1 = date1->month;
    int month2 = date2->month;
    int year1 = date1->year;
    int year2 = date2->year;

    if (year1 == year2)
    {
        if (month1 == month2)
        {
            if (day1 == day2)
            {
                return 0;
            }
            else if (day1 < day2)
            {
                return -1;
            }
            else
            {
                return 1;
            }
        }
        else if (month1 < month2)
        {
            return -1;
        }
        else
        {
            return 1;
        }
    }
    else if (year1 < year2)
    {
        return -1;
    }
    else
    {
        return 1;
    }
}

void date_destroy(Date *d)
{
    free(d);
    d = NULL;
}
