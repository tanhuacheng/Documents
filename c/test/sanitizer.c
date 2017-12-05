int main(int argc, char *argv[])
{
    (void)argc;
    (void)argv;

    char *ptr;
    {
        char my_char = 56;
        ptr = &my_char; // error: address use after scope!
    }

    *ptr = 123;

    return *ptr;
}
