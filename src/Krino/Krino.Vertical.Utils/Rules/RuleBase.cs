using System;

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

        // To ensure derived classes provide it.
        public override int GetHashCode() => throw new NotImplementedException();


        public static bool operator ==(RuleBase<T> rule1, RuleBase<T> rule2) => Equals(rule1, rule2);
        public static bool operator !=(RuleBase<T> rule1, RuleBase<T> rule2) => !Equals(rule1, rule2);


        public static AndRule<T> operator &(RuleBase<T> rule1, IRule<T> rule2) => rule1.And(rule2);

        public static OrRule<T> operator |(RuleBase<T> rule1, IRule<T> rule2) => rule1.Or(rule2);

        public static NotRule<T> operator !(RuleBase<T> rule) => rule.Not();
    }
}
