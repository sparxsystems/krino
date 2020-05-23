using System;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Helper functionality to build a composition of rules.
    /// </summary>
    public static class RuleMaker
    {
        /// <summary>
        /// Creates a rule expecting the provided value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns></returns>
        public static IsRule<T> Is<T>(T value) => new IsRule<T>(value);

        /// <summary>
        /// Creates the rule accepting everything except the provided value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns></returns>
        public static NotRule<T> IsNot<T>(T value) => new IsRule<T>(value).Not();

        /// <summary>
        /// Creates the rule accepting the null value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static IsNullRule<T> IsNull<T>() where T : class  => new IsNullRule<T>();

        /// <summary>
        /// Creates the rule accepting the value which is not null.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static NotRule<T> IsNotNull<T>() where T : class => new IsNullRule<T>().Not();

        /// <summary>
        /// Creates the rule which accept all values i.e. always returns true.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static AnythingRule<T> Anything<T>() => new AnythingRule<T>();

        /// <summary>
        /// Creates the rule which does not accept any value i.e. always returns false.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public static NothingRule<T> Nothing<T>() => new NothingRule<T>();

        /// <summary>
        /// Logical operator AND.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule1"></param>
        /// <param name="rule2"></param>
        /// <returns></returns>
        public static AndRule<T> And<T>(this IRule<T> rule1, IRule<T> rule2) => new AndRule<T>(rule1, rule2);

        /// <summary>
        /// Logical operator AND.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule1"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        public static AndRule<T> And<T>(this IRule<T> rule1, T value) where T : IEquatable<T> => rule1.And(new IsRule<T>(value));

        /// <summary>
        /// Logical operator OR.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule1"></param>
        /// <param name="rule2"></param>
        /// <returns></returns>
        public static OrRule<T> Or<T>(this IRule<T> rule1, IRule<T> rule2) => new OrRule<T>(rule1, rule2);

        /// <summary>
        /// Logical operator OR.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule1"></param>
        /// <param name="value"></param>
        /// <returns></returns>
        public static OrRule<T> Or<T>(this IRule<T> rule1, T value) where T : IEquatable<T> => rule1.Or(new IsRule<T>(value));

        /// <summary>
        /// Logical operator NOT (negation).
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule"></param>
        /// <returns></returns>
        public static NotRule<T> Not<T>(this IRule<T> rule) => new NotRule<T>(rule);

        /// <summary>
        /// Logical operator NOT (negation).
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <returns></returns>
        public static NotRule<T> Not<T>(T value) where T : IEquatable<T> => new IsRule<T>(value).Not();
    }
}
