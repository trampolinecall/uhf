interface Callable<A, R> {
    call(arg: A): R;
}

class Int {
    constructor(public value: number) {}
}
class Float {
    constructor(public value: number) {}
}
class Char {
    constructor(public value: string) {}
}
class UHFString {
    constructor(public value: string) {}
}
class Bool {
    constructor(public value: boolean) {}
}
class Tuple<A, B> {
    constructor(public first: A, public second: B) {}
}

let __uhf_intrinsic_str_concat: Callable<UHFString, Callable<UHFString, UHFString>> = {
    call: (a: UHFString) => {
        return {
            call: (b: UHFString) => new UHFString(a.value + b.value),
        };
    }
};

let __uhf_intrinsic_int_add: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value + b.value),
        };
    }
};
let __uhf_intrinsic_int_sub: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value - b.value),
        };
    }
};
let __uhf_intrinsic_int_mul: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value * b.value),
        };
    }
};
let __uhf_intrinsic_int_div: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value / b.value),
        };
    }
};
let __uhf_intrinsic_int_mod: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value % b.value),
        };
    }
};

let __uhf_intrinsic_impure_print: Callable<UHFString, Int> = {
    call: (a: UHFString) => {
        console.log(a.value);
        return new Int(0);
    }
}
