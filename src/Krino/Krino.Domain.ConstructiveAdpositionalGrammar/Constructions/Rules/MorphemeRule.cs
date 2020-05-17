using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes;
using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Rule to eveluate if something (e.g. morpheme inside an adtree element) matches the morpheme.
    /// </summary>
    [DebuggerDisplay("{GrammarCharacter}: {MorphRule}")]
    public class MorphemeRule : IEquatable<MorphemeRule>
    {
        public static MorphemeRule Nothing => new MorphemeRule(GrammarCharacter.Epsilon, MorphRuleMaker.Nothing, MaskRule.Nothing);

        public static MorphemeRule Any(GrammarCharacter grammarCharacter) => new MorphemeRule(grammarCharacter, MorphRuleMaker.Anything, MaskRule.Anything);

        public static MorphemeRule Is(GrammarCharacter grammarCharacter, string morph, BigInteger attributes) => new MorphemeRule(grammarCharacter, MorphRuleMaker.Is(morph), MaskRule.Is(attributes));

        public static MorphemeRule O => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Anything, MaskRule.Is(Attributes.O));

        public static MorphemeRule O_Lexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.O.Lexeme));

        public static MorphemeRule O_NonLexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.O.NonLexeme));

        public static MorphemeRule O_Not_NonLexeme => new MorphemeRule(GrammarCharacter.O, MorphRuleMaker.Anything, MaskRule.Is(Attributes.O) & !MaskRule.Is(Attributes.O.NonLexeme));

        public static MorphemeRule I => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Anything, MaskRule.Is(Attributes.I));

        public static MorphemeRule I_Lexeme => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.I.Lexeme));

        public static MorphemeRule I_NonLexeme => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.I.NonLexeme));

        public static MorphemeRule I_Not_NonLexeme => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.Anything, MaskRule.Is(Attributes.I) & !MaskRule.Is(Attributes.I.NonLexeme));

        public static MorphemeRule I1 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.I.Lexeme.Verb.Monovalent));
        public static MorphemeRule I2 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.I.Lexeme.Verb.Bivalent));
        public static MorphemeRule I3 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.I.Lexeme.Verb.Trivalent));
        public static MorphemeRule I4 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.I.Lexeme.Verb.Quadrivalent));
        public static MorphemeRule I5 => new MorphemeRule(GrammarCharacter.I, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.I.Lexeme.Verb.Pentavalent));
        public static MorphemeRule A => new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.Anything, MaskRule.Is(Attributes.A));

        public static MorphemeRule A_Lexeme => new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.A.Lexeme));

        public static MorphemeRule A_Not_NonLexeme => new MorphemeRule(GrammarCharacter.A, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.A) & !MaskRule.Is(Attributes.A.NonLexeme));

        public static MorphemeRule E_Preposition => new MorphemeRule(GrammarCharacter.E, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.E.Lexeme.Preposition));
        public static MorphemeRule E_Adverb => new MorphemeRule(GrammarCharacter.E, MorphRuleMaker.NotEmptyString, MaskRule.Is(Attributes.E.Lexeme.Adverb));
        public static MorphemeRule U => new MorphemeRule(GrammarCharacter.U, MorphRuleMaker.Anything, MaskRule.Is(Attributes.U));
        public static MorphemeRule Epsilon => new MorphemeRule(GrammarCharacter.Epsilon, MorphRuleMaker.Nothing, MaskRule.Is(Attributes.Epsilon));


        public IRule<string> MorphRule { get; private set; }
        public GrammarCharacter GrammarCharacter { get; private set; }
        public IRule<BigInteger> AttributesRule { get; private set; }

        public MorphemeRule(GrammarCharacter grammarCharacter, IRule<string> morphRule, IRule<BigInteger> attributesRule)
        {
            GrammarCharacter = grammarCharacter;
            MorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            AttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
        }

        /// <summary>
        /// Checks if the morph and morphemeAttributes match the morpheme rule.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="morphemeAttributes"></param>
        /// <returns></returns>
        public bool IsMatch(string morph, BigInteger morphemeAttributes)
        {
            bool result = MorphRule.Evaluate(morph) && AttributesRule.Evaluate(morphemeAttributes);
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
            hash = (hash * 16777619) ^ AttributesRule.GetHashCode();

            return hash;
        }
    }
}
