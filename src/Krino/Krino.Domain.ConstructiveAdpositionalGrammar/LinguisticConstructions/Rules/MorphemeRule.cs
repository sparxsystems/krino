using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Attributing;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.Rules
{
    /// <summary>
    /// Rule evaluating morphemes.
    /// </summary>
    [DebuggerDisplay("{GrammarCharacter}: {MorphRule}")]
    public class MorphemeRule : IEquatable<MorphemeRule>
    {
        public static MorphemeRule Is(string morph, BigInteger attributes) => Is(MorphRuleMaker.Is(morph), attributes, 0);

        public static MorphemeRule Is(IRule<string> morphRule, BigInteger attributes) => Is(morphRule, attributes, 0);

        public static MorphemeRule Is(IRule<string> morphRule, BigInteger attributes, BigInteger notAttributes) =>
            notAttributes != 0 ?
                new MorphemeRule(GrammarCharacterExt.GetGrammarCharacter(attributes), morphRule, MaskRule.Is(attributes) & !MaskRule.Is(notAttributes)) :
                new MorphemeRule(GrammarCharacterExt.GetGrammarCharacter(attributes), morphRule, MaskRule.Is(attributes));


        public static MorphemeRule Anything => new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.Anything, MaskRule.Anything);

        public static MorphemeRule Nothing => new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.Nothing, MaskRule.Nothing);

        public static MorphemeRule O_Lexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Something, MaskRule.Is(Attributes.O.Lexeme));

        public static MorphemeRule O_NonLexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Something, MaskRule.Is(Attributes.O.NonLexeme));

        public static MorphemeRule I_Lexeme => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.Lexeme));

        public static MorphemeRule A_Lexeme => new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.Something, MaskRule.Is(Attributes.A.Lexeme));

        public static MorphemeRule E_Lexeme => new MorphemeRule(GrammarCharacter.E, MorphRuleMaker.Something, MaskRule.Is(Attributes.E.Lexeme));


        public static MorphemeRule U_Lexeme => new MorphemeRule(GrammarCharacter.U, MorphRuleMaker.Something, MaskRule.Is(Attributes.U.Lexeme));
        public static MorphemeRule U_NonLexeme => new MorphemeRule(GrammarCharacter.U, MorphRuleMaker.Something, MaskRule.Is(Attributes.U.NonLexeme));

        public static MorphemeRule Epsilon => new MorphemeRule(GrammarCharacter.e, MorphRuleMaker.EmptyString, MaskRule.Is(Attributes.Epsilon));


        public MorphemeRule(GrammarCharacter grammarCharacter, IRule<string> morphRule, IRule<BigInteger> attributesRule)
        {
            GrammarCharacter = grammarCharacter;
            MorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            AttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
        }


        public IRule<string> MorphRule { get; private set; }
        public GrammarCharacter GrammarCharacter { get; private set; }
        public IRule<BigInteger> AttributesRule { get; private set; }

        public IRule<GrammarCharacter> InheritanceRule { get; private set; } = InheritanceRuleMaker.Epsilon_U_E;

        public int ValencyPosition { get; private set; }

        public int Order { get; private set; } = int.MaxValue;

        public MorphemeRule SetValencyPosition(int valencyPosition)
        {
            ValencyPosition = valencyPosition;
            return this;
        }

        public MorphemeRule SetOrder(int order)
        {
            Order = order;
            return this;
        }

        public MorphemeRule SetAttributes(BigInteger attributes)
        {
            AttributesRule = MaskRule.Is(attributes);
            return this;
        }

        public MorphemeRule SetInheritance(IRule<GrammarCharacter> inheritanceRule)
        {
            InheritanceRule = inheritanceRule;
            return this;
        }

        

        /// <summary>
        /// Checks if the morpheme matches the rule.
        /// </summary>
        /// <param name="morpheme"></param>
        /// <returns></returns>
        public bool Evaluate(Morpheme morpheme)
        {
            bool result = MorphRule.Evaluate(morpheme.Morph) && AttributesRule.Evaluate(morpheme.Attributes);
            return result;
        }

        public bool Equals(MorphemeRule other)
        {
            bool result = MorphRule.Equals(other.MorphRule) &&
                          AttributesRule.Equals(other.AttributesRule);
            return result;
        }

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ MorphRule.GetHashCode();
            hash = (hash * 16777619) ^ GrammarCharacter.GetHashCode();
            hash = (hash * 16777619) ^ AttributesRule.GetHashCode();
            hash = (hash * 16777619) ^ ValencyPosition.GetHashCode();
            hash = (hash * 16777619) ^ Order.GetHashCode();

            return hash;
        }

    }
}
