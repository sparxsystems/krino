using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Vertical.Utils.Enums;
using Krino.Vertical.Utils.Rules;
using System;
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
        /// Accepts pattern which morpheme can be accepted into the adposition.
        /// </summary>
        public static ExpressionRule<Pattern> ByUpMorphemeRule(Pattern parent) => Expression(x => CanConnectPatternToAdPosition(parent, x));

        /// <summary>
        /// Accepts pattern which morpheme would be accepted on right.
        /// </summary>
        /// <param name="parent"></param>
        /// <returns></returns>
        public static ExpressionRule<Pattern> ByRightMorphemeRule(Pattern parent) => Expression(x => CanConnectPatternToRight(parent, x));


        /// <summary>
        /// Accepts pattern which morpheme would be accepted on left.
        /// </summary>
        /// <param name="parent"></param>
        /// <returns></returns>
        public static ExpressionRule<Pattern> ByLeftMorphemeRule(Pattern parent) => Expression(x => CanConnectPatternToLeft(parent, x));

        /// <summary>
        /// Accepts a particular pattern.
        /// </summary>
        /// <param name="pattern"></param>
        /// <returns></returns>
        public static IsRule<Pattern> Is(Pattern pattern) => RuleMaker.Is(pattern);

        /// <summary>
        /// Accepts morphematic adposition pattern with specified grammar characters.
        /// </summary>
        /// <param name="grammarCharacterRule"></param>
        /// <returns></returns>
        public static ExpressionRule<Pattern> MorphematicAdPosition(IRule<GrammarCharacter> grammarCharacterRule) => Expression(x => CanConnectMorphematicAdPosition(grammarCharacterRule, x));

        public static ExpressionRule<Pattern> NoneMorphematicAdPosition => Expression(x => !x.IsMorphematicAdPosition);

        public static ExpressionRule<Pattern> IsMorpheme => Expression(x => x.IsMorpheme);

        public static ExpressionRule<Pattern> Expression(Func<Pattern, bool> evaluate) => RuleMaker.Expression<Pattern>(x => evaluate(x));

        private static bool CanConnectMorphematicAdPosition(IRule<GrammarCharacter> grammarCharacterRule, Pattern child)
        {
            var result = true;

            if (child.IsMorphematicAdPosition)
            {
                result = grammarCharacterRule.Evaluate(child.UpRule.GrammarCharacter);
            }

            return result;
        }



        private static bool CanConnectPatternToAdPosition(Pattern parent, Pattern child)
        {
            bool result = false;

            if (child.UpRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
            {
                result = parent.UpRule.AttributesRule.Evaluate(childAttributesValueRule.Value);

                if (result)
                {
                    if (child.UpRule.MorphRule is IValueRule<string> childMorphValueRule && childMorphValueRule.Value != "")
                    {
                        result = parent.UpRule.MorphRule.Evaluate(childMorphValueRule.Value);
                    }
                    else if (parent.UpRule.MorphRule is IValueRule<string>)
                    {
                        // Prent is a rule for a particular morph string, but the child is a generic rule.
                        result = false;
                    }
                }
            }

            return result;
        }

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
                if (child.IsLikeMorpheme || child.IsGrammarAdPosition)
                {
                    if (child.UpRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                    {
                        result = parent.RightRule.AttributesRule.Evaluate(childAttributesValueRule.Value);

                        if (result)
                        {
                            if (child.UpRule.MorphRule is IValueRule<string> childMorphValueRule && childMorphValueRule.Value != "")
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
                else if (child.IsMorphematicAdPosition)
                {
                    if (child.RightRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                    {
                        result = parent.RightRule.AttributesRule.Evaluate(childAttributesValueRule.Value);
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
            else if (child.IsLikeMorpheme || child.IsGrammarAdPosition)
            {
                if (child.UpRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                {
                    result = parent.LeftRule.AttributesRule.Evaluate(childAttributesValueRule.Value);

                    if (result)
                    {
                        // Note: in case of GrammarAdPosition the morph is "".
                        if (child.UpRule.MorphRule is IValueRule<string> childMorphValueRule && childMorphValueRule.Value != "")
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
            else if (child.IsMorphematicAdPosition)
            {
                // Note: get the driving grammar character of the child from the right branch.
                if (child.RightRule.AttributesRule is IValueRule<BigInteger> childAttributesValueRule)
                {
                    result = parent.LeftRule.AttributesRule.Evaluate(childAttributesValueRule.Value);
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
