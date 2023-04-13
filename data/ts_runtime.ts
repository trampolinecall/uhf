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

type SwitchMatcher<T> = (a: Thunk<T>) => boolean;
function bool_literal_matcher(expecting: boolean): SwitchMatcher<Bool> {
    return (b: Thunk<Bool>) => {
        return b.get_value().value == expecting;
    }
}
function default_matcher<T>(): SwitchMatcher<T> {
    return (_: Thunk<T>) => {
        return true;
    }
}
// TODO: same todo about extends Showable constraint
function tuple_matcher<A extends Showable, B extends Showable>(): SwitchMatcher<Tuple<A, B>> {
    return (_: Thunk<Tuple<A, B>>) => {
        return true;
    }
}
class SwitchEvaluator<E, R> implements Evaluator<R> {
    constructor(public testing: Thunk<E>, public arms: [SwitchMatcher<E>, Thunk<R>][]) {}

    evaluate(): R {
        for (let [matcher, result] of this.arms) {
            if (matcher(this.testing)) {
                return result.get_value();
            }
        }

        throw Error("non-exhaustive matchers in switch");
    }
}

class SeqEvaluator<A, B> implements Evaluator<B> {
    constructor(public a: Thunk<A>, public b: Thunk<B>) {}

    evaluate(): B {
        this.a.get_value();
        return this.b.get_value();
    }
}

class FunctionEvaluator<T> implements Evaluator<T> {
    constructor(public f: () => T) {}

    evaluate(): T {
        return this.f();
    }
}
