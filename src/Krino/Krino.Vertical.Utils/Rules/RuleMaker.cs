using System;
using System.Collections.Generic;
using System.Linq.Expressions;

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
        /// <param name="comparer"></param>
        /// <returns></returns>
        public static IsRule<T> Is<T>(T value, IEqualityComparer<T> comparer = null) => new IsRule<T>(value, comparer);

        /// <summary>
        /// Creates the rule accepting everything except the provided value.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="value"></param>
        /// <param name="comparer"></param>
        /// <returns></returns>
        public static NotRule<T> IsNot<T>(T value, IEqualityComparer<T> comparer = null) => new IsRule<T>(value, comparer).Not();

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
        /// Creates the rule accepting any value from the list.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="comparer"></param>
        /// <param name="items"></param>
        /// <returns></returns>
        public static ContainsRule<T> Contains<T>(IEqualityComparer<T> comparer = null, params T[] items) => Contains(items, comparer);

        /// <summary>
        /// Creates the rule accepting any value from the list.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="items"></param>
        /// <param name="comparer"></param>
        /// <returns></returns>
        public static ContainsRule<T> Contains<T>(IEnumerable<T> items, IEqualityComparer<T> comparer = null) => new ContainsRule<T>(items, comparer);

        /// <summary>
        /// Creates the rule which accept values for which the expression returns true.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="expression"></param>
        /// <returns></returns>
        public static ExpressionRule<T> Expression<T>(Expression<Func<T, bool>> expression) => new ExpressionRule<T>(expression);

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
        /// <param name="comparer"></param>
        /// <returns></returns>
        public static AndRule<T> And<T>(this IRule<T> rule1, T value, IEqualityComparer<T> comparer = null) where T : IEquatable<T> => rule1.And(new IsRule<T>(value, comparer));

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
        /// <param name="comparer"></param>
        /// <returns></returns>
        public static OrRule<T> Or<T>(this IRule<T> rule1, T value, IEqualityComparer<T> comparer = null) where T : IEquatable<T> => rule1.Or(new IsRule<T>(value, comparer));

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
        /// <param name="comparer"></param>
        /// <returns></returns>
        public static NotRule<T> Not<T>(T value, IEqualityComparer<T> comparer = null) where T : IEquatable<T> => new IsRule<T>(value, comparer).Not();


        public static BeginsWithStrRule BeginsWithStr(string beginningOfStr) => new BeginsWithStrRule(beginningOfStr);
        public static EndsWithStrRule EndsWithStr(string endOfString) => new EndsWithStrRule(endOfString);

        public static RuleBase<string> EndsWithOneOfStr(params string[] suffixes)
        {
            RuleBase<string> result = null;

            for (int i = 0; i < suffixes.Length; ++i)
            {
                var suffix = suffixes[i];

                if (i == 0)
                {
                    result = EndsWithStr(suffix);
                }
                else
                {
                    result |= EndsWithStr(suffix);
                }
            }

            return result;
        }
    }
}
