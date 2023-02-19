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
