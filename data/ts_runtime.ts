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

type SwitchMatcher<T> = (a: T) => boolean;
function bool_literal_matcher(expecting: boolean): SwitchMatcher<Bool> {
    return (b: Bool) => {
        return b.value == expecting;
    }
}
function default_matcher<T>(): SwitchMatcher<T> {
    return (_: T) => {
        return true;
    }
}
// TODO: same todo about extends Showable constraint
function tuple_matcher<A extends Showable, B extends Showable>(): SwitchMatcher<Tuple<A, B>> {
    return (_: Tuple<A, B>) => {
        return true;
    }
}
function adt_matcher(discr: string): SwitchMatcher<{ data: { discriminant: string} }> {
    return (d: { data: { discriminant: string } }) => {
        return d.data.discriminant === discr;
    }
}
