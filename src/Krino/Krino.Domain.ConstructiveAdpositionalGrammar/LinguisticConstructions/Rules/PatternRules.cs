using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules
{
    public static class PatternRules
    {
        /// <summary>
        /// All patterns are accepted.
        /// </summary>
        public static IRule<Pattern> Anything => RuleMaker.Anything<Pattern>();

        /// <summary>
        /// It does not accept any pattern.
        /// </summary>
        public static NothingRule<Pattern> Nothing => RuleMaker.Nothing<Pattern>();

        /// <summary>
        /// Accepts pattern which morpheme would be accepted on right.
        /// </summary>
        /// <param name="parent"></param>
        /// <returns></returns>
        public static IExpressionRule<Pattern> ByRightMorphemeRule(Pattern parent) => RuleMaker.Expression<Pattern>(x => CanConnectPatternToRight(parent, x));


        /// <summary>
        /// Accepts pattern which morpheme would be accepted on left.
        /// </summary>
        /// <param name="parent"></param>
        /// <returns></returns>
        public static IExpressionRule<Pattern> ByLeftMorphemeRule(Pattern parent) => RuleMaker.Expression<Pattern>(x => CanConnectPatternToLeft(parent, x));


        private static bool CanConnectPatternToRight(Pattern parent, Pattern child)
        {
            bool result = false;

            if (parent.RightRule.Equals(MorphemeRule.Anything))
            {
                result = true;
            }
            else if (parent.ValencyPosition == 0 || parent.ValencyPosition == child.ValencyPosition + 1)
            {
                if (parent.RightRule.AttributesRule is IValueRule<BigInteger> parentAttributesValueRule)
                {
                    if (child.IsLikeMorpheme)
                    {
                        if (child.UpRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                        {
                            result = EnumBase.IsIn(parentAttributesValueRule.Value, childAttributesValueRule.Value);

                            if (result)
                            {
                                if (child.UpRule.MorphRule is IValueRule<string> childMorphValueRule)
                                {
                                    result = parent.RightRule.MorphRule.Evaluate(childMorphValueRule.Value);
                                }
                                else if (parent.RightRule.MorphRule is IValueRule<string>)
                                {
                                    // Prent is a rule for a particular morph string, but the child is a generic rule.
                                    // Therefore child may accept also morphe strings which are not accepted byt the parent.
                                    result = false;
                                }
                            }
                        }
                    }
                    else if (child.RightRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                    {
                        result = EnumBase.IsIn(parentAttributesValueRule.Value, childAttributesValueRule.Value);
                    }
                }
                else if (parent.RightRule.GrammarCharacter == child.RightRule.GrammarCharacter)
                {
                    result = true;
                }
            }
            
            return result;
        }

        private static bool CanConnectPatternToLeft(Pattern parent, Pattern child)
        {
            bool result = false;

            if (parent.LeftRule.Equals(MorphemeRule.Anything))
            {
                result = true;
            }
            else if (parent.LeftRule.AttributesRule is IValueRule<BigInteger> parentAttributesValueRule)
            {
                if (child.IsLikeMorpheme)
                {
                    if (child.UpRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                    {
                        result = EnumBase.IsIn(parentAttributesValueRule.Value, childAttributesValueRule.Value);

                        if (result)
                        {
                            if (child.UpRule.MorphRule is IValueRule<string> childMorphValueRule)
                            {
                                result = parent.LeftRule.MorphRule.Evaluate(childMorphValueRule.Value);
                            }
                            else if (parent.LeftRule.MorphRule is IValueRule<string>)
                            {
                                // Prent is a rule for a particular morph string, but the child is a generic rule.
                                // Therefore child may accept also morphe strings which are not accepted byt the parent.
                                result = false;
                            }
                        }
                    }
                }
                else if (child.LeftRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                {
                    result = EnumBase.IsIn(parentAttributesValueRule.Value, childAttributesValueRule.Value);
                }
            }
            else if (parent.LeftRule.GrammarCharacter == child.LeftRule.GrammarCharacter)
            {
                result = true;
            }

            return result;
        }
    }
}
