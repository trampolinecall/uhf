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

function istr_concat(a: UHFString, b: UHFString) {
    return new UHFString(a.value + b.value);
}

let iint_add: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value + b.value),
        };
    }
};
let iint_sub: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value - b.value),
        };
    }
};
let iint_mul: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value * b.value),
        };
    }
};
let iint_div: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value / b.value),
        };
    }
};
let iint_mod: Callable<Int, Callable<Int, Int>> = {
    call: (a: Int) => {
        return {
            call: (b: Int) => new Int(a.value % b.value),
        };
    }
};

let iimpure_print: Callable<UHFString, Int> = {
    call: (a: UHFString) => {
        console.log(a.value);
        return new Int(0);
    }
}
