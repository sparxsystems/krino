using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Rule evaluating morphemes.
    /// </summary>
    [DebuggerDisplay("{GrammarCharacter}: {MorphRule}")]
    public class MorphemeRule : IEquatable<MorphemeRule>
    {
        public static MorphemeRule Is(string morph, BigInteger attributes) => new MorphemeRule(GrammarCharacterExt.GetGrammarCharacter(attributes), MorphRuleMaker.Is(morph), MaskRule.Is(attributes));

        public static MorphemeRule Is(IRule<string> morphRule, BigInteger attributes) => new MorphemeRule(GrammarCharacterExt.GetGrammarCharacter(attributes), morphRule, MaskRule.Is(attributes));

        public static MorphemeRule Anything => new MorphemeRule(GrammarCharacter.Epsilon, MorphRuleMaker.Anything, MaskRule.Anything);

        public static MorphemeRule Nothing => new MorphemeRule(GrammarCharacter.Epsilon, MorphRuleMaker.Nothing, MaskRule.Nothing);

        public static MorphemeRule Any(GrammarCharacter grammarCharacter) => new MorphemeRule(grammarCharacter, MorphRuleMaker.Anything, MaskRule.Anything);

        public static MorphemeRule Is(GrammarCharacter grammarCharacter, string morph, BigInteger attributes) => new MorphemeRule(grammarCharacter, MorphRuleMaker.Is(morph), MaskRule.Is(attributes));

        public static MorphemeRule O => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Anything, MaskRule.Is(Attributes.O));

        public static MorphemeRule O_Lexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Something, MaskRule.Is(Attributes.O.Lexeme));

        public static MorphemeRule O_NonLexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Something, MaskRule.Is(Attributes.O.NonLexeme));

        public static MorphemeRule O_Not_NonLexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Anything, MaskRule.Is(Attributes.O) & !MaskRule.Is(Attributes.O.NonLexeme));

        public static MorphemeRule I => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Anything, MaskRule.Is(Attributes.I));

        public static MorphemeRule I_Lexeme => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.Lexeme));

        public static MorphemeRule I_NonLexeme => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.NonLexeme));

        public static MorphemeRule I_Not_NonLexeme => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Anything, MaskRule.Is(Attributes.I) & !MaskRule.Is(Attributes.I.NonLexeme));

        public static MorphemeRule I1 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.Lexeme.Verb.Monovalent));
        public static MorphemeRule I2 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.Lexeme.Verb.Bivalent));
        public static MorphemeRule I3 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.Lexeme.Verb.Trivalent));
        public static MorphemeRule I4 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.Lexeme.Verb.Quadrivalent));
        public static MorphemeRule I5 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Something, MaskRule.Is(Attributes.I.Lexeme.Verb.Pentavalent));
        public static MorphemeRule A => new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.Anything, MaskRule.Is(Attributes.A));

        public static MorphemeRule A_Lexeme => new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.Something, MaskRule.Is(Attributes.A.Lexeme));

        public static MorphemeRule A_Not_NonLexeme => new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.Anything, MaskRule.Is(Attributes.A) & !MaskRule.Is(Attributes.A.NonLexeme));

        public static MorphemeRule E_Lexeme => new MorphemeRule(GrammarCharacter.E, MorphRuleMaker.Something, MaskRule.Is(Attributes.E.Lexeme));
        public static MorphemeRule E_Not_NonLexeme => new MorphemeRule(GrammarCharacter.E, MorphRuleMaker.Anything, MaskRule.Is(Attributes.E) & !MaskRule.Is(Attributes.E.NonLexeme));

        public static MorphemeRule E_Adverb => new MorphemeRule(GrammarCharacter.E, MorphRuleMaker.Something, MaskRule.Is(Attributes.E.Lexeme.Adverb));
        public static MorphemeRule U => new MorphemeRule(GrammarCharacter.U, MorphRuleMaker.Anything, MaskRule.Is(Attributes.U));
        public static MorphemeRule U_Lexeme => new MorphemeRule(GrammarCharacter.U, MorphRuleMaker.Something, MaskRule.Is(Attributes.U.Lexeme));
        public static MorphemeRule U_Not_NonLexeme => new MorphemeRule(GrammarCharacter.U, MorphRuleMaker.Anything, MaskRule.Is(Attributes.U) & !MaskRule.Is(Attributes.U.NonLexeme));
        public static MorphemeRule Epsilon => new MorphemeRule(GrammarCharacter.Epsilon, MorphRuleMaker.EmptyString, MaskRule.Is(Attributes.Epsilon));


        public static MorphemeRule Something(GrammarCharacter grammarCharacter) => new MorphemeRule(grammarCharacter, MorphRuleMaker.Something, MaskRule.Is(grammarCharacter.GetAttributes()));


        public IRule<string> MorphRule { get; private set; }
        public GrammarCharacter GrammarCharacter { get; private set; }
        public IRule<BigInteger> AttributesRule { get; private set; }

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

        public BigInteger GetAttributes()
        {
            BigInteger result;
            if (AttributesRule is IReferenceValueRule<BigInteger> valueRule)
            {
                result = valueRule.ReferenceValue;
            }
            else
            {
                result = GrammarCharacter.GetAttributes();
            }

            return result;
        }

        public MorphemeRule(GrammarCharacter grammarCharacter, IRule<string> morphRule, IRule<BigInteger> attributesRule)
        {
            GrammarCharacter = grammarCharacter;
            MorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            AttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
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
