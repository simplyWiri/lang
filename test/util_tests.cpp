#include <doctest.h>
#include "util.h"

TEST_SUITE_BEGIN("Utility Tests");

TEST_CASE("Tagged Pointer") {
    int integerValue = 5;
    util::t_ptr<int, float> intPointer{&integerValue};

    CHECK(intPointer.holds<int>());
    CHECK_EQ(*intPointer.get<int>(), integerValue);
    CHECK_EQ(intPointer.get<float>(), nullptr);

    float floatValue = 6.0;
    util::t_ptr<int, float> floatPointer{&floatValue};

    CHECK(floatPointer.holds<float>());
    CHECK_EQ(*floatPointer.get<float>(), floatValue);
    CHECK_EQ(floatPointer.get<int>(), nullptr);

    SUBCASE("operator=") {
        util::t_ptr<int, float> a;
        a = intPointer;
        CHECK(a.holds<int>());

        util::t_ptr<int, float> b;
        b = floatPointer;
        CHECK(b.holds<float>());
    }
}

TEST_SUITE_END();