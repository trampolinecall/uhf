interface Lambda<A, R> {
    call(arg: A): R;
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

class TupleEvaluator<A, B> implements Evaluator<[Thunk<A>, Thunk<B>]> {
    constructor(public a: Thunk<A>, public b: Thunk<B>) {}

    evaluate(): [Thunk<A>, Thunk<B>] {
        return [this.a, this.b];
    }
}

class CallEvaluator<A, R> implements Evaluator<R> {
    constructor(public callee: Thunk<Lambda<Thunk<A>, Thunk<R>>>, public arg: Thunk<A>) {}

    evaluate(): R {
        return this.callee.get_value().call(this.arg).get_value();
    }
}

class TupleDestructure1Evaluator<A> implements Evaluator<A> {
    constructor(public tuple: Thunk<[Thunk<A>, Thunk<any>]>) {}

    evaluate(): A {
        return this.tuple.get_value()[0].get_value();
    }
}

class TupleDestructure2Evaluator<B> implements Evaluator<B> {
    constructor(public tuple: Thunk<[Thunk<any>, Thunk<B>]>) {}

    evaluate(): B {
        return this.tuple.get_value()[1].get_value();
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
class TupleMatcher<A, B> implements Matcher<[A, B]> {
    matches(t: [A, B]): boolean {
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
