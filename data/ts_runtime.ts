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

