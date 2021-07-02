namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Abstract class declaring convenient logical operators for rules.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public abstract class RuleBase<T> : IRule<T>
    {
        public abstract bool Equals(IRule<T> other);

        public abstract bool Evaluate(T value);

        public override bool Equals(object obj) => obj is IRule<T> objRule && Equals(objRule);

        public override int GetHashCode() => -1;


        public static AndRule<T> operator &(RuleBase<T> rule1, IRule<T> rule2) => rule1.And(rule2);

        public static OrRule<T> operator |(RuleBase<T> rule1, IRule<T> rule2) => rule1.Or(rule2);

        public static NotRule<T> operator !(RuleBase<T> rule) => rule.Not();
    }
}
