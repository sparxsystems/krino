﻿using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using Krino.Vertical.Utils.Rules;
using System;
using System.Diagnostics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions.Rules
{
    /// <summary>
    /// Rule to eveluate if something (e.g. morpheme inside an adtree element) matches the morpheme.
    /// </summary>
    [DebuggerDisplay("{myMorphRule} && {myAttributesRule}")]
    public class MorphemeRule : IEquatable<MorphemeRule>
    {
        // Note: it is the struct to avoid incosistent situations if the MorphemeRule is null.

        public static MorphemeRule Anything = new MorphemeRule(Rule.Anything<string>(), Rule.Anything<ulong>());
        public static MorphemeRule Nothing = new MorphemeRule(Rule.Nothing<string>(), Rule.Nothing<ulong>());
        public static MorphemeRule O = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.O));
        public static MorphemeRule I = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.I));
        public static MorphemeRule I1 = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.I.Verb.Modal));
        public static MorphemeRule I2 = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.I.Verb.Bivalent));
        public static MorphemeRule I3 = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.I.Verb.Trivalent));
        public static MorphemeRule I4 = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.I.Verb.Quadrivalent));
        public static MorphemeRule I5 = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.I.Verb.Pentavalent));
        public static MorphemeRule A = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.A));
        public static MorphemeRule E_Preposition = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.E.Preposition));
        public static MorphemeRule E_Adverb = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.E.Adverb));
        public static MorphemeRule U = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.U));
        public static MorphemeRule Epsilon = new MorphemeRule(Rule.Anything<string>(), new MaskRule(StructuralAttributes.Epsilon));


        private IRule<string> myMorphRule;
        private IRule<ulong> myAttributesRule;

        public MorphemeRule(IRule<string> morphRule, IRule<ulong> attributesRule)
        {
            myMorphRule = morphRule ?? throw new ArgumentNullException(nameof(morphRule));
            myAttributesRule = attributesRule ?? throw new ArgumentNullException(nameof(attributesRule));
        }

        /// <summary>
        /// Checks if the morph and morphemeAttributes match the morpheme rule.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="morphemeAttributes"></param>
        /// <returns></returns>
        public bool IsMatch(string morph, ulong morphemeAttributes)
        {
            bool result = myMorphRule.Evaluate(morph) && myAttributesRule.Evaluate(morphemeAttributes);
            return result;
        }


        public bool Equals(MorphemeRule other)
        {
            bool result = myMorphRule.Equals(other.myMorphRule) &&
                          myAttributesRule.Equals(other.myAttributesRule);
            return result;
        }

        public override bool Equals(object obj) => obj is MorphemeRule rule && Equals(rule);

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ myMorphRule.GetHashCode();
            hash = (hash * 16777619) ^ myAttributesRule.GetHashCode();

            return hash;
        }

    }
}
