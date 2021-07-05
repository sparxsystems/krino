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
            else if (parent.RightRule.Equals(MorphemeRule.Nothing))
            {
                return false;
            }
            // If valency order is ok.
            else if (parent.ValencyPosition == 0 || parent.ValencyPosition == child.ValencyPosition + 1)
            {
                // If the connecting child is a morpheme.
                if (child.IsLikeMorpheme)
                {
                    if (child.UpRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                    {
                        result = parent.RightRule.AttributesRule.Evaluate(childAttributesValueRule.Value);

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
                else if (child.IsMorphematicAdPosition())
                {
                    // If the parent can accept such morphematic adposition.
                    if (parent.RightRule.MorphematicAdPositionRule.Evaluate(child.UpRule.GrammarCharacter))
                    {
                        if (child.RightRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                        {
                            result = parent.RightRule.AttributesRule.Evaluate(childAttributesValueRule.Value);
                        }
                    }
                }
                else if (child.RightRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                {
                    result = parent.RightRule.AttributesRule.Evaluate(childAttributesValueRule.Value);
                }
                else
                {
                    result = parent.RightRule.GrammarCharacter == child.RightRule.GrammarCharacter;
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
            else if (parent.LeftRule.Equals(MorphemeRule.Nothing))
            {
                return false;
            }
            else if (child.IsLikeMorpheme)
            {
                if (child.UpRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                {
                    result = parent.LeftRule.AttributesRule.Evaluate(childAttributesValueRule.Value);

                    if (result)
                    {
                        if (child.UpRule.MorphRule is IValueRule<string> childMorphValueRule)
                        {
                            result = parent.LeftRule.MorphRule.Evaluate(childMorphValueRule.Value);
                        }
                        else if (parent.LeftRule.MorphRule is IValueRule<string>)
                        {
                            // Prent is a rule for a particular morph string, but the child is a generic rule.
                            // Therefore child may accept also morph strings which are not accepted byt the parent.
                            result = false;
                        }
                    }
                }
            }
            else if (child.IsMorphematicAdPosition())
            {
                if (parent.LeftRule.MorphematicAdPositionRule.Evaluate(child.UpRule.GrammarCharacter))
                {
                    // Note: get the driving grammar character of the child from the right branch.
                    if (child.RightRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                    {
                        result = parent.LeftRule.AttributesRule.Evaluate(childAttributesValueRule.Value);
                    }
                }
            }
            // Note: get the driving grammar character of the child from the right branch.
            else if (child.RightRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
            {
                result = parent.LeftRule.AttributesRule.Evaluate(childAttributesValueRule.Value);
            }
            // Note: get the driving grammar character of the child from the right branch.
            else if (parent.LeftRule.GrammarCharacter == child.RightRule.GrammarCharacter)
            {
                result = true;
            }

            return result;
        }
    }
}
