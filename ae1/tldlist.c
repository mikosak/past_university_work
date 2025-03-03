#include "tldlist.h"
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

TLDNode *create_node(Date *date, char *domain);
void recursive_insert(TLDNode *node, char *domain, Date *date);
TLDNode *destroy_nodes(TLDNode *root);
void flatten(TLDNode *root);

struct tldlist
{
    TLDNode *root;
    long node_count;
    Date *begin, *end;
};

struct tldnode
{
    Date *date;
    char *domain;
    TLDNode *left, *right;
    long count;
};

struct tlditerator
{
    TLDList *tld;
    TLDNode *curr_node, *last_returned;
};

TLDList *tldlist_create(Date *begin, Date *end)
{
    if (!begin || !end) {
        return NULL;
    }
    TLDList *list = malloc(sizeof *list);
    if (!list)
    {
        return NULL;
    }
    list->root = NULL;
    list->node_count = 0;
    list->begin = begin;
    list->end = end;
    return list;
}

void tldlist_destroy(TLDList *tld)
{
    destroy_nodes(tld->root);
    free(tld);
    tld = NULL;
}

int tldlist_add(TLDList *tld, char *hostname, Date *d)
{
    for (int i = 0; hostname[i]; i++)
    {
        hostname[i] = tolower(hostname[i]);
    }
    char *domain_tmp = strrchr(hostname, '.') + 1;
    char *domain = strdup(domain_tmp);
    if (domain && date_compare(d, tld->begin) > -1 && date_compare(d, tld->end) < 1)
    {
        if (!tld->root)
        {
            tld->root = create_node(d, domain);
        }
        else
        {
            recursive_insert(tld->root, domain, d);
        }
        tld->node_count += 1;
        return 1;
    }
    return 0;
}

long tldlist_count(TLDList *tld)
{
    return tld->node_count;
}

TLDIterator *tldlist_iter_create(TLDList *tld)
{
    TLDIterator *iterator = malloc(sizeof *iterator);
    if (!iterator)
    {
        return NULL;
    }
    flatten(tld->root);
    iterator->tld = tld;
    iterator->curr_node = tld->root;
    iterator->last_returned = NULL;
    return iterator;
}

TLDNode *tldlist_iter_next(TLDIterator *iter)
{
    if (iter->curr_node && iter->curr_node->right)
    {
        if (iter->last_returned == iter->curr_node)
            {
                iter->curr_node = iter->curr_node->right;
            }
    iter->last_returned = iter->curr_node;
    return iter->curr_node;
    }
    else
    {
        return NULL;
    }
}

void tldlist_iter_destroy(TLDIterator *iter)
{
    free(iter);
    iter = NULL;
}

char *tldnode_tldname(TLDNode *node)
{
    return node->domain;
}

long tldnode_count(TLDNode *node)
{
    return node->count;
}

/*
 * create_node is a helper function that returns a new node if
 * given the date and domain; returns NULL if unsuccessful, pointer
 * to the new node otherwise.
 */
TLDNode *create_node(Date *date, char *domain)
{
    TLDNode *node = malloc(sizeof *node);
    node->date = date;
    node->domain = domain;
    node->count = 1;
    node->left = NULL;
    node->right = NULL;
    return node;
}

/*
 * recursive_insert is a helper function that recursively adds nodes
 * based off their domain value, fulfilling the binary tree rules; does
 * not return a value.
 */
void recursive_insert(TLDNode *node, char *domain, Date *date)
{
    if (strcmp(node->domain, domain) == 0)
    { // equal
        node->count += 1;
        free(domain);
        domain = NULL;
    }
    else if (strcmp(node->domain, domain) < 0)
    { // left
        if (!node->left)
        {
            TLDNode *new_node = create_node(date, domain);
            node->left = new_node;
        }
        else
        {
            recursive_insert(node->left, domain, date);
        }
    }
    else
    { // right
        if (!node->right)
        {
            TLDNode *new_node = create_node(date, domain);
            node->right = new_node;
        }
        else
        {
            recursive_insert(node->right, domain, date);
        }
    }
}

/*
 * destroy_nodes is a helper function that recursively
 * frees nodes, including their domain. no need to free
 * dates as date_destroy is called in  tldmonitor. Returns
 * NULL after recursion is finished
 */
TLDNode *destroy_nodes(TLDNode *root)
{
    if (!root)
    {
        return NULL;
    }
    else
    {
        destroy_nodes(root->left);
        destroy_nodes(root->right);
        free(root->domain);
        root->domain = NULL;
        free(root);
        root = NULL;
    }
    return root;
}

/*
 * flatten is a helper function that recursively
 * flattens a tree when given the root of the tree
 * to flatten, by making all pointers to the left
 * point to NULL and making the right pointer point
 * to the next node. Returns nothing.
 */
void flatten(TLDNode *root)
{
    if (!root || (!root->left && !root->right))
    {
        return;
    }

    if (root->left)
    {
        flatten(root->left);
        TLDNode *tmp = root->right;
        root->right = root->left;
        root->left = NULL;
        TLDNode *next_right = root->right;

        while (next_right->right)
        {
            next_right = next_right->right;
        }
        next_right->right = tmp;
    }
    flatten(root->right);
}
