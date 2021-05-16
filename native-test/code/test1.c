#define WITH_STR(expr) expr, #expr

void printf(string s);
void printf_i(string s, int a);
void printf_s_i(string s, string a, int b);
void printf_s_i_i(string s, string a, int b, int c);
void printf_s_s(string s, string a, string b);
void printf_s_s_s(string s, string a, string b, string c);

int failure = 0;

int main() {
    printf("start test\n");
    assert_int(0, WITH_STR(0));
    assert_int(42, WITH_STR(42));
    assert_int(21, WITH_STR(5+20-4));
    assert_int(47, WITH_STR(5+6*7));
    assert_int(15, WITH_STR(5*(9-6)));
    assert_int(4, WITH_STR((3+5)/2));
    assert_int(10, WITH_STR(-10+20));
    assert_int(1, WITH_STR((-1-2)/-3));
    assert_bool(true, WITH_STR(99==99));
    assert_bool(false, WITH_STR(1==0));
    assert_bool(false, WITH_STR(99!=99));
    assert_bool(true, WITH_STR(1!=0));
    assert_bool(true, WITH_STR((5*(9-6)==15)==true));
    assert_bool(true, WITH_STR(-1<989));
    assert_bool(false, WITH_STR(1000-1<=989));
    assert_bool(true, WITH_STR(1000-11<=989));
    assert_bool(true, WITH_STR(1+1+1+1>1));
    assert_bool(false, WITH_STR(1>1+1+1+1));
    assert_bool(true, WITH_STR(1+1+1+1>=1));
    assert_bool(false, WITH_STR(1>=1+1+1+1));
    assert_bool(true, WITH_STR(1+1+1+1>=1+1+1+1));

    if (failure == 0) {
        printf("all test passed.\n");
    } else {
        printf_i("%d tests failed.\n", failure);
    }

    return failure;
}

void foo() { printf("OK\n"); }

int add(int a, int b) { return a + b; }

bool assert_int(int expected, int actual, string expr) {
    if (actual == expected) {
        printf_s_i("OK %s => %d\n", expr, actual);
        return true;
    } else {
        printf_s_i_i("NG %s => %d expected, but got %d\n", expr, expected, actual);
        failure = failure + 1;
    }
    return false;
}

bool assert_bool(bool expected, bool actual, string expr) {
    if (actual == expected) {
        printf_s_s("OK %s => %s\n", expr, show_bool(actual));
        return true;
    } else {
        printf_s_s_s("NG %s => %s expected, but got %s\n", expr, show_bool(expected), show_bool(actual));
        failure = failure + 1;
    }
    return false;
}

string show_bool(bool a) {
    if (a) {
        return "true";
    }
    return "false";
}
