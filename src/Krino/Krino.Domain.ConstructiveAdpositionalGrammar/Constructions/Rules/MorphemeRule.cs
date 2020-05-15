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
    [DebuggerDisplay("{GrammarCharacter}: {MorphRule} && {AttributesRule}")]
    public class MorphemeRule : IEquatable<MorphemeRule>
    {
        public static MorphemeRule Anything => new MorphemeRule(GrammarCharacter.Epsilon, Rule.Anything<string>(), Rule.Anything<BigInteger>());
        public static MorphemeRule Nothing => new MorphemeRule(GrammarCharacter.Epsilon, Rule.Nothing<string>(), Rule.Nothing<BigInteger>());
        public static MorphemeRule O => new MorphemeRule(GrammarCharacter.O, Rule.Anything<string>(), new MaskRule(Attributes.O));
        public static MorphemeRule I => new MorphemeRule(GrammarCharacter.I, Rule.Anything<string>(), new MaskRule(Attributes.I));
        public static MorphemeRule I1 => new MorphemeRule(GrammarCharacter.I, Rule.Anything<string>(), new MaskRule(Attributes.I.Verb.Modal));
        public static MorphemeRule I2 => new MorphemeRule(GrammarCharacter.I, Rule.Anything<string>(), new MaskRule(Attributes.I.Verb.Bivalent));
        public static MorphemeRule I3 => new MorphemeRule(GrammarCharacter.I, Rule.Anything<string>(), new MaskRule(Attributes.I.Verb.Trivalent));
        public static MorphemeRule I4 => new MorphemeRule(GrammarCharacter.I, Rule.Anything<string>(), new MaskRule(Attributes.I.Verb.Quadrivalent));
        public static MorphemeRule I5 => new MorphemeRule(GrammarCharacter.I, Rule.Anything<string>(), new MaskRule(Attributes.I.Verb.Pentavalent));
        public static MorphemeRule A => new MorphemeRule(GrammarCharacter.A, Rule.Anything<string>(), new MaskRule(Attributes.A));
        public static MorphemeRule E_Preposition => new MorphemeRule(GrammarCharacter.E, Rule.Anything<string>(), new MaskRule(Attributes.E.Preposition));
        public static MorphemeRule E_Adverb => new MorphemeRule(GrammarCharacter.E, Rule.Anything<string>(), new MaskRule(Attributes.E.Adverb));
        public static MorphemeRule U => new MorphemeRule(GrammarCharacter.U, Rule.Anything<string>(), new MaskRule(Attributes.U));
        public static MorphemeRule Epsilon => new MorphemeRule(GrammarCharacter.Epsilon, Rule.Nothing<string>(), new MaskRule(Attributes.Epsilon));


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
