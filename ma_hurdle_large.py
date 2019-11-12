#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
#include <stdbool.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_permutation.h>

// Implementation of simple (mu+lambda)~EA
// Author: Phan Trung Hai Nguyen
// Last update: 16/11/2018

// individual representation

struct ind {
    int *val;
    float fitness;
    struct ind *next;
};

int ma(int mu, int lmbda, float p, int n, float optimal, int hurdle, int id, bool ls, int depth);
struct ind *create_ind(int *val, float fitness);
struct ind *add_ind(struct ind *head, struct ind *newind);
struct ind *sort_pop(struct ind *x);
struct ind *find_middle(struct ind *x);
struct ind *merge(struct ind *x, struct ind *y);
float evaluator(int *val, int n, int hurdle);
bool float_compare(float a, float b);
void free_ind(struct ind *p);
void fils(int depth, int *val, int n, int hurdle);
void bils(int depth, int *val, int n, int hurdle, gsl_rng * r);

int ma(int mu, int lmbda, float p, int n, float optimal, int hurdle, int id, bool ls, int depth) {

    // random number generator	
    const gsl_rng_type * T;
    gsl_rng * r;
    gsl_rng_env_setup();
    T = gsl_rng_default;
    r = gsl_rng_alloc(T);
    gsl_rng_set(r, (unsigned long) n * id * time(NULL));

    //population of mu individuals
    struct ind *pop = NULL;

    //iteration counter
    int t = 0;

    //sample the initial population
    int i, j;
    for (i = 0; i < mu; i++) {
        int *val = (int *) malloc(n * sizeof (int));
        for (j = 0; j < n; j++) {
            float rand = gsl_rng_uniform(r);
            val[j] = (rand <= 0.5) ? 1 : 0;
        }
        float fitness = evaluator(val, n, hurdle);
        pop = add_ind(pop, create_ind(val, fitness));
    }

    // iterative process    
    while (1) {

        t++;
        int sel_indx;
        struct ind *sel_ind = NULL;
        struct ind *curr_ind = NULL;
        struct ind *del = NULL;

        // create lambda offspring
        for (i = 0; i < lmbda; i++) {
            sel_ind = pop;
            if (mu > 1) {
                sel_indx = gsl_rng_uniform_int(r, mu);
            } else {
                sel_indx = 0;
            }
            for (j = 1; j <= sel_indx; j++) {
                sel_ind = sel_ind->next;
            }
            int *new_val = (int *) malloc(n * sizeof (int));
            for (j = 0; j < n; j++) {
                float rand = gsl_rng_uniform(r);
                new_val[j] = (rand <= p) ? 1 - (sel_ind->val[j]) : sel_ind->val[j];
            }

            //call local search 
            if (ls) { //first improvement
                fils(depth, new_val, n, hurdle);
            } else { //best improvement
                bils(depth, new_val, n, hurdle, r);
            }

            // add new individual to population
            while (sel_ind->next != NULL) {
                sel_ind = sel_ind->next;
            }
            float fitness = evaluator(new_val, n, hurdle);
            sel_ind->next = create_ind(new_val, fitness);
        }

        //sort population according to fitness
        pop = sort_pop(pop);

        //select the fittest mu individuals
        curr_ind = pop;
        for (i = 1; i < mu; i++) {
            curr_ind = curr_ind->next;
        }

        del = curr_ind->next;
        curr_ind->next = NULL;
        free_ind(del);

        // terminate?
        float fittest = pop->fitness;
        if (float_compare(optimal, fittest)) {
            break;
        }
    }

    //release the memory allocated
    free_ind(pop);

    return mu + t*lmbda;
}

void fils(int depth, int *val, int n, int hurdle) {
    int i, j, k;
    bool flag;
    int *per = NULL;
    int *y = (int *) malloc(n * sizeof (int));

    // create random permutation
    const size_t N = n;
    const gsl_rng_type * T;
    gsl_rng * r;
    gsl_permutation * p = gsl_permutation_alloc(N);
    gsl_rng_env_setup();
    T = gsl_rng_default;
    r = gsl_rng_alloc(T);
    gsl_permutation_init(p);

    for (i = 0; i < depth; i++) {
        for (k = 0; k < n; k++) {
            y[k] = val[k];
        }
        gsl_ran_shuffle(r, p->data, N, sizeof (size_t));

        //random permutation
        per = (int *) p->data;

        flag = false;
        for (j = 0; j < n; j++) {
            y[per[j]] = 1 - y[per[j]];
            float y_fitness = evaluator(y, n, hurdle);
            float val_fitness = evaluator(val, n, hurdle);
            if (y_fitness > val_fitness) {
                for (k = 0; k < n; k++) {
                    val[k] = y[k];
                }
                flag = true;
                break;
            }
        }
        if (!flag) {
            break;
        }
    }
    free(y);
}

void bils(int depth, int *val, int n, int hurdle, gsl_rng * r) {
    int *y = (int *) malloc(n * sizeof (int));
    int i, j, k;
    float curr_best;
    float y_fitness, val_fitness;
    int count;

    for (i = 0; i < depth; i++) {
        for (k = 0; k < n; k++) {
            y[k] = val[k];
        }
        count = 0;
        struct ind *cur_best_inds = NULL;
        curr_best = evaluator(val, n, hurdle);

        for (j = 0; j < n; j++) {
            y[j] = 1 - y[j];
            y_fitness = evaluator(y, n, hurdle);
            val_fitness = evaluator(val, n, hurdle);
            if (y_fitness > val_fitness) {
                cur_best_inds = create_ind(y, y_fitness);
                curr_best = y_fitness;
                count = 1;
            } else if (float_compare(y_fitness, curr_best)) {
                cur_best_inds = add_ind(cur_best_inds, create_ind(y, y_fitness));
                count++;
            }
            y[j] = 1 - y[j];
        }

        if (cur_best_inds == NULL) {
            break;
        } else {
            struct ind *curr = cur_best_inds;
            int rand = gsl_rng_uniform_int(r, count);
            for (j = 1; j <= rand; j++) {
                curr = curr->next;
            }
            for (j = 1; j < n; j++) {
                val[j] = curr->val[j];
            }
            free_ind(cur_best_inds);
        }
    }
    free(y);
}

// compare two floating-point numbers

bool float_compare(float a, float b) {
    float epsilon = 0.00001;
    return fabs(a - b) < epsilon;
}

// fitness evaluation

float evaluator(int *val, int n, int hurdle) {
    int z = 0;
    int i = 0;
    for (; i < n; i++) {
        if (val[i] == 0) z++;
    }
    return (float) -ceil(((float) z) / hurdle) - ((float) (z % hurdle)) / hurdle;
}

//create a dictionary

struct ind *create_ind(int *val, float fitness) {
    struct ind *p = (struct ind *) malloc(sizeof (struct ind));
    if (p == NULL) {
        return NULL;
    }
    p->val = val;
    p->fitness = fitness;
    p->next = NULL;
    return p;
}

//add a new dict into the current dict

struct ind *add_ind(struct ind *head, struct ind *new) {
    if (head == NULL) {
        return new;
    }
    if (new != NULL) {
        new->next = head;
        head = new;
    }
    return head;
}

//free the memory

void free_ind(struct ind *head) {
    struct ind *tmp;
    while (head != NULL) {
        tmp = head;
        head = head->next;
        free(tmp->val);
        free(tmp);
    }
}

//sort a dictionary

struct ind *sort_pop(struct ind *x) {
    struct ind *y = NULL;
    struct ind *m = NULL;
    if (x == NULL || x->next == NULL) {
        return x;
    }
    m = find_middle(x);
    y = m->next;
    m->next = NULL;
    return merge(sort_pop(x), sort_pop(y));
}

//find the middle elem in a dictionary

struct ind *find_middle(struct ind *x) {
    struct ind *slow = x;
    struct ind *fast = x;
    while (fast->next != NULL && fast->next->next != NULL) {
        slow = slow->next;
        fast = fast->next->next;
    }
    return slow;
}

//merge two dicts 

struct ind *merge(struct ind *x, struct ind *y) {
    struct ind *tmp = NULL;
    struct ind *head = NULL;
    struct ind *curr = NULL;
    if (x == NULL) {
        head = y;
    } else if (y == NULL) {
        head = x;
    } else {
        while (x != NULL && y != NULL) {
            // Swap x and y if x is not largest.
            if (x->fitness < y->fitness) {
                tmp = y;
                y = x;
                x = tmp;
            }
            if (head == NULL) { // First element?
                head = x;
                curr = x;
            } else {
                curr->next = x;
                curr = curr->next;
            }
            x = x->next;
        }
        // Either x or y is empty.
        if (x != NULL) {
            curr->next = x;
        } else if (y != NULL) {
            curr->next = y;
        }
    }
    return head;
}

// main method

int main(int argc, char **argv) {

    if (argc != 9) {
        printf("USAGE: %s mu lmbda rate n hurdle id local_search depth\n", argv[0]);
        exit(1);
    }

    int mu = atoi(argv[1]);
    int lmbda = atoi(argv[2]);
    int rate = atoi(argv[3]);
    int n = atoi(argv[4]);
    int hurdle = atoi(argv[5]);
    int id = atoi(argv[6]);
    int ls = atoi(argv[7]);
    int depth = atoi(argv[8]);

    bool fils = (ls == 1) ? true : false;

    float p = ((float) rate) / n;

    // optimal fitness
    float optimal = 0.0;

    int res = ma(mu, lmbda, p, n, optimal, hurdle, id, fils, depth);

    printf("%d \n", res);

    return (0);
}