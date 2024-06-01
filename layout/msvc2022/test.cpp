
class BaseA_0 {
public:
    int field1;
};

class BaseA_1 {
public:
    int field1;
    virtual void func1() {}
};

class BaseB_0 {
public:
    int field2;
};

class BaseB_1 {
public:
    int field2;
    virtual void func2() {}
};

class Derived_A0_B0_0 : public BaseA_0, public BaseB_0 {
public:
    int derivedField;
};

class Derived_A0_B0_1 : public BaseA_0, public BaseB_0 {
public:
    int derivedField;
    virtual void func3() {}
};

class Derived_A0_B1_0 : public BaseA_0, public BaseB_1 {
public:
    int derivedField;
};

class Derived_A0_B1_1 : public BaseA_0, public BaseB_1 {
public:
    int derivedField;
    virtual void func3() {}
};

class Derived_A1_B0_0 : public BaseA_1, public BaseB_0 {
public:
    int derivedField;
};

class Derived_A1_B0_1 : public BaseA_1, public BaseB_0 {
public:
    int derivedField;
    virtual void func3() {}
};

class Derived_A1_B1_0 : public BaseA_1, public BaseB_1 {
public:
    int derivedField;
};

class Derived_A1_B1_1 : public BaseA_1, public BaseB_1 {
public:
    int derivedField;
    virtual void func3() {}
};