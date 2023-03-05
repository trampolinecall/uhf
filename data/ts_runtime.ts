interface Lambda<A, R> {
    call(arg: Thunk<A>): Thunk<R>;
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
        return this.value;
    }
}
class UHFString {
    constructor(public value: string) {}

    show(): string {
        return this.value;
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
    constructor(public first: Thunk<A>, public second: Thunk<B>) {}

    show(): string {
        return `(${this.first.get_value().show()}, ${this.second.get_value().show()})`;
    }
}

interface Evaluator<T> {
    evaluate(): T;
}

class Thunk<T> {
    value: T | null;
    evaluator: Evaluator<T>;

    constructor(evaluator: Evaluator<T>) {
        this.value = null;
        this.evaluator = evaluator;
    }

    get_value(): T {
        if (this.value === null) {
            this.value = this.evaluator.evaluate();
        }

        return this.value;
    }
}

class ConstEvaluator<T> implements Evaluator<T> {
    constructor(public value: T) {}

    evaluate(): T {
        return this.value;
    }
}

class PassthroughEvaluator<T> implements Evaluator<T> {
    constructor(public other: Thunk<T>) {}

    evaluate(): T {
        return this.other.get_value();
    }
}

// TODO: this should also not have bounds on the type parameters
class TupleEvaluator<A extends Showable, B extends Showable> implements Evaluator<Tuple<A, B>> {
    constructor(public a: Thunk<A>, public b: Thunk<B>) {}

    evaluate(): Tuple<A, B> {
        return new Tuple(this.a, this.b);
    }
}

class CallEvaluator<A, R> implements Evaluator<R> {
    constructor(public callee: Thunk<Lambda<A, R>>, public arg: Thunk<A>) {}

    evaluate(): R {
        return this.callee.get_value().call(this.arg).get_value();
    }
}

class TupleDestructure1Evaluator<A extends Showable> implements Evaluator<A> {
    constructor(public tuple: Thunk<Tuple<A, any>>) {}

    evaluate(): A {
        return this.tuple.get_value().first.get_value();
    }
}

class TupleDestructure2Evaluator<B extends Showable> implements Evaluator<B> {
    constructor(public tuple: Thunk<Tuple<any, B>>) {}

    evaluate(): B {
        return this.tuple.get_value().second.get_value();
    }
}

interface Matcher<T> {
    matches(thing: T): boolean;
}
class BoolLiteralMatcher implements Matcher<boolean> {
    constructor(public expecting: boolean) {}
    matches(t: boolean): boolean {
        return t == this.expecting;
    }
}
class DefaultMatcher<T> implements Matcher<T> {
    matches(t: T): boolean {
        return true;
    }
}
// TODO: same todo about extends Showable constraint
class TupleMatcher<A extends Showable, B extends Showable> implements Matcher<Tuple<A, B>> {
    matches(t: Tuple<A, B>): boolean {
        return true; // tuples only have one constructor so it always matches
    }
}
class SwitchEvaluator<E, R> implements Evaluator<R> {
    constructor(public testing: Thunk<E>, public arms: [Matcher<E>, Thunk<R>][]) {}

    evaluate(): R {
        let testing = this.testing.get_value();
        for (let [matcher, result] of this.arms) {
            if (matcher.matches(testing)) {
                return result.get_value();
            }
        }

        throw Error("non-exhaustive matchers in switch");
    }
}
