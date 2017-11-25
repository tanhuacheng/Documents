#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define IS_DECIMAL(x) ((x) >= 0 && (x) <= 8)

static void sudoku_block_position (int i, int j, int* x, int* y)
{
    *x = (i / 3) * 3 + (j / 3);
    *y = (i % 3) * 3 + (j % 3);
}

static int sudoku_check_row (const int (*a)[9], int row)
{
    for (int i = 0; i < 9; i++) {
        if (!a[row][i]) {
            continue;
        }
        for (int j = i + 1; j < 9; j++) {
            if (a[row][i] % 10 == a[row][j] % 10) {
                return -1;
            }
        }
    }

    return 0;
}

static int sudoku_check_col (const int (*a)[9], int col)
{
    for (int i = 0; i < 9; i++) {
        if (!a[i][col]) {
            continue;
        }
        for (int j = i + 1; j < 9; j++) {
            if (a[i][col] % 10 == a[j][col] % 10) {
                return -1;
            }
        }
    }

    return 0;
}

static int sudoku_check_blk (const int (*a)[9], int blk)
{
    int x0, y0;
    int x1, y1;

    for (int i = 0; i < 9; i++) {
        sudoku_block_position(blk, i, &x0, &y0);
        if (!a[x0][y0]) {
            continue;
        }
        for (int j = i + 1; j < 9; j++) {
            sudoku_block_position(blk, j, &x1, &y1);
            if (a[x0][y0] % 10 == a[x1][y1] % 10) {
                return -1;
            }
        }
    }

    return 0;
}

static int sudoku_check (const int (*a)[9])
{
    for (int i = 0; i < 9; i++) {
        if (sudoku_check_row(a, i) < 0 ||
            sudoku_check_col(a, i) < 0 ||
            sudoku_check_blk(a, i) < 0)
        {
            return -1;
        }
    }

    return 0;
}

static void sudoku_dump (const int (*a)[9])
{
    printf("\n");
    printf("+---------+---------+---------+\n");
    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 9; j++) {
            if (!j) {
                printf("|");
            }
            if (a[i][j] < 10) {
                printf(" %c ", a[i][j] ? a[i][j] + '0' : '?');
            } else {
                printf(" \033[1;31m%c\033[0m ", a[i][j] % 10 + '0');
            }
            if (j % 3 == 2) {
                printf("|");
            }
        }
        printf("\n");
        if (i % 3 == 2 && i != 8) {
            printf("|---------+---------+---------|\n");
        }
    }
    printf("+---------+---------+---------+\n");
    printf("\n");
}

static void sudoku_init (int (*a)[9])
{
    int x, y, z;

    printf("intput: (x, y) => z > ");
    while (scanf(" %d %d %d", &x, &y, &z) != EOF) {
        if (!IS_DECIMAL(x) || !IS_DECIMAL(y) || (!IS_DECIMAL(z - 1) && z != 0)) {
            printf("invalid values\n");
            continue;
        }
        a[x][y] = z ? z + 10 : z;
        printf("intput: (x, y) => z > ");
    }
    printf("\n");

    sudoku_dump(a);
    if (sudoku_check(a) < 0) {
        printf("Invalid sudoku, program terminated ...\n");
        exit(0);
    }
}

static int sudoku_solve_inner (int (*a)[9], int x, int y)
{
    static int depth = 0;

    if (x > 8) {
        printf("depth: %d\n", depth);
        return 0;
    }
    depth++;

    if (a[x][y]) {
        x = (y + 1) / 9 + x;
        y = (y + 1) % 9;
        return sudoku_solve_inner(a, x, y);
    }

    for (int i = 1; i < 10; i++) {
        a[x][y] = i;
        if (sudoku_check_row(a, x) < 0 ||
            sudoku_check_col(a, y) < 0 ||
            sudoku_check_blk(a, x - (x % 3) + y / 3) < 0)
        {
            continue;
        }
        if (sudoku_solve_inner(a, x, y) < 0) {
            continue;
        }
        return 0;
    }

    a[x][y] = 0;

    return -1;
}

static int sudoku_solve (int (*a)[9])
{
    return sudoku_solve_inner(a, 0, 0);
}

int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    int a[9][9] = {
        { 0, 0, 5,  8, 0, 0,  0, 0, 0 },
        { 0, 0, 0,  0, 0, 1,  6, 0, 0 },
        { 4, 0, 0,  0, 9, 2,  0, 8, 0 },

        { 0, 0, 9,  0, 0, 4,  3, 0, 1 },
        { 0, 1, 0,  0, 0, 0,  0, 2, 0 },
        { 0, 0, 2,  0, 0, 8,  9, 0, 5 },

        { 7, 0, 0,  0, 2, 6,  0, 5, 0 },
        { 0, 0, 0,  0, 0, 5,  4, 0, 0 },
        { 0, 0, 6,  3, 0, 0,  0, 0, 0 },
    };

    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 9; j++) {
            if (a[i][j]) {
                a[i][j] += 10;
            }
        }
    }

// #if 0
//     srand(time(NULL));
//     a[0][0] = rand() % 9 + 1;
// #else
//     sudoku_init(a);
// #endif

    if (sudoku_solve(a) < 0) {
        printf("Can not solve this problem!\n");
        exit(1);
    }

    printf("Solved:\n");
    sudoku_dump(a);

    return 0;
}
