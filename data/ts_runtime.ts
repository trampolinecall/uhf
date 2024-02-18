interface Lambda<A, R> {
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

function iint_add(a: Int, b: Int) {
    return new Int(a.value + b.value);
}
function iint_sub(a: Int, b: Int) {
    return new Int(a.value - b.value);
}
function iint_mul(a: Int, b: Int) {
    return new Int(a.value * b.value);
}
function iint_div(a: Int, b: Int) {
    return new Int(Math.floor(a.value / b.value));
}
function iint_mod(a: Int, b: Int) {
    return new Int(a.value % b.value);
}
