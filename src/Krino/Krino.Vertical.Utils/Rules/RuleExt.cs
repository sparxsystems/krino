using System.Collections.Generic;
using System.Linq;

namespace Krino.Vertical.Utils.Rules
{
    /// <summary>
    /// Utility and extenstion functionality for rules.
    /// </summary>
    public static class RuleExt
    {
        /// <summary>
        /// Returns true if the parentRule returns same results as this rule.
        /// </summary>
        /// <remarks>
        /// The method works only for rules which consists of:
        /// AndRule, OrRule, NotRule, IsRule, AnythingRule and Nothing
        /// </remarks>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule"></param>
        /// <param name="parentRule"></param>
        /// <returns></returns>
        public static bool IsSubruleOf<T>(this IRule<T> rule, IRule<T> parentRule)
        {
            if (rule.Equals(parentRule))
            {
                return true;
            }

            if (parentRule is NothingRule<T>)
            {
                return false;
            }

            if (parentRule is AnythingRule<T> && rule is NothingRule<T>)
            {
                return false;
            }

            IEnumerable<T> requiredValues = rule.GetReferenceValues();
            if (requiredValues.Any())
            {
                foreach (T value in requiredValues)
                {
                    bool ruleResult = rule.Evaluate(value);
                    bool parentRuleResult = parentRule.Evaluate(value);
                    if (ruleResult != parentRuleResult)
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        /// <summary>
        /// Traces the rule and extract the RequiredValue properties from all IsRule.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule"></param>
        /// <returns></returns>
        public static IEnumerable<T> GetReferenceValues<T>(this IRule<T> rule)
        {
            IEnumerable<T> result = rule.GetRules().OfType<IReferenceValueRule<T>>().Select(x => x.ReferenceValue);
            return result;
        }

        /// <summary>
        /// Returns all rules the provided rule consists of.
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <param name="rule"></param>
        /// <returns></returns>
        public static IEnumerable<IRule<T>> GetRules<T>(this IRule<T> rule)
        {
            Stack<IRule<T>> stack = new Stack<IRule<T>>();
            stack.Push(rule);

            while (stack.Count > 0)
            {
                IRule<T> thisRule = stack.Pop();

                yield return thisRule;

                if (thisRule is IBinaryOperatorRule<T> binaryRule)
                {
                    stack.Push(binaryRule.Subrule2);
                    stack.Push(binaryRule.Subrule1);
                }
                else if (thisRule is IUnaryOperatorRule<T> unaryRule)
                {
                    stack.Push(unaryRule.Subrule);
                }
            }
        }
    }
}
