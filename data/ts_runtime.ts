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

class TupleDestructure1Evaluator<A, B> implements Evaluator<A> {
    constructor(public tuple: Thunk<[Thunk<A>, Thunk<B>]>) {}

    evaluate(): A {
        return this.tuple.get_value()[0].get_value();
    }
}

class TupleDestructure2Evaluator<A, B> implements Evaluator<B> {
    constructor(public tuple: Thunk<[Thunk<A>, Thunk<B>]>) {}

    evaluate(): B {
        return this.tuple.get_value()[1].get_value();
    }
}
