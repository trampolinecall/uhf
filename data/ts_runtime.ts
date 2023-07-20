interface Lambda<A, R> {
    call(arg: A): R;
}

// TODO: make this a typeclass in uhf
interface Showable {
    show(): string;
}

class Int {
    constructor(public value: number) {}

    show(): string {
        return this.value.toString();
    }
}
class Float {
    constructor(public value: number) {}

    show(): string {
        return this.value.toString();
    }
}
class Char {
    constructor(public value: string) {}

    show(): string {
        return `'${this.value}'`;
    }
}
class UHFString {
    constructor(public value: string) {}

    show(): string {
        return `"${this.value.toString()}"`;
    }
}
class Bool {
    constructor(public value: boolean) {}

    show(): string {
        return this.value.toString();
    }
}
// TODO: this should not have these bounds on the parameters
class Tuple<A extends Showable, B extends Showable> {
    constructor(public first: A, public second: B) {}

    show(): string {
        return `(${this.first.show()}, ${this.second.show()})`;
    }
}

