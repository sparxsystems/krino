using Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement;
using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Constructions
{
    /// <summary>
    /// Represents data of the morpheme rules.
    /// </summary>
    public struct MorphemeRule
    {
        // Note: it is the struct to avoid incosistent situations if the MorphemeRule is null.

        public static MorphemeRule Anything = default;
        public static MorphemeRule Nothing = new MorphemeRule(null, null, 0, ulong.MaxValue);
        public static MorphemeRule EmptyMorph = new MorphemeRule("", null, 0, 0);
        public static MorphemeRule O = new MorphemeRule(null, null, StructuralAttributes.O, 0);
        public static MorphemeRule I = new MorphemeRule(null, null, StructuralAttributes.I, 0);
        public static MorphemeRule I1 = new MorphemeRule(null, null, StructuralAttributes.I.Verb.Monovalent, 0);
        public static MorphemeRule I2 = new MorphemeRule(null, null, StructuralAttributes.I.Verb.Bivalent, 0);
        public static MorphemeRule I3 = new MorphemeRule(null, null, StructuralAttributes.I.Verb.Trivalent, 0);
        public static MorphemeRule I4 = new MorphemeRule(null, null, StructuralAttributes.I.Verb.Quadrivalent, 0);
        public static MorphemeRule I5 = new MorphemeRule(null, null, StructuralAttributes.I.Verb.Pentavalent, 0);
        public static MorphemeRule A = new MorphemeRule(null, null, StructuralAttributes.A, 0);
        public static MorphemeRule E = new MorphemeRule(null, null, StructuralAttributes.E, 0);
        public static MorphemeRule U = new MorphemeRule(null, null, StructuralAttributes.U, 0);
        public static MorphemeRule Epsilon = new MorphemeRule("", null, StructuralAttributes.Epsilon, 0);
        

        public MorphemeRule(string requiredMorph, string rejectedMorph, ulong requiredAttributes, ulong rejectedAttributes)
        {
            RequiredMorph = requiredMorph;
            RejectedMorph = rejectedMorph;
            RequiredAttributes = requiredAttributes;
            RejectedAttributes = rejectedAttributes;
        }

        /// <summary>
        /// If set then it specifies the required morph.
        /// </summary>
        public string RequiredMorph { get; private set; }

        /// <summary>
        /// If set then it specifies the morph which is not allowed.
        /// </summary>
        public string RejectedMorph { get; private set; }

        /// <summary>
        /// The required morpheme attributes.
        /// </summary>
        public ulong RequiredAttributes { get; private set; }

        /// <summary>
        /// The morpheme attributes which are not allowed.
        /// </summary>
        public ulong RejectedAttributes { get; private set; }

        /// <summary>
        /// Checks if the morph and morphemeAttributes match the morpheme rule.
        /// </summary>
        /// <param name="morph"></param>
        /// <param name="morphemeAttributes"></param>
        /// <returns></returns>
        public bool IsMatch(string morph, ulong morphemeAttributes)
        {
            bool isMatch = CheckMorphs(RequiredMorph, morph);
            if (isMatch)
            {
                if (RejectedMorph != null)
                {
                    isMatch = !CheckMorphs(RejectedMorph, morph);
                }
            }

            if (isMatch)
            {
                isMatch = EnumBase.IsIn(RequiredAttributes, morphemeAttributes);
                if (isMatch && RejectedAttributes != 0)
                {
                    isMatch = !EnumBase.IsIn(RejectedAttributes, morphemeAttributes);
                }
            }

            return isMatch;
        }

        private bool CheckMorphs(string expectedMorph, string morphToCheck)
        {
            bool isMatch = true;

            if (expectedMorph != null)
            {
                if (expectedMorph != morphToCheck)
                {
                    isMatch = false;
                }
            }

            return isMatch;
        }

        public override bool Equals(object obj) => obj is MorphemeRule rule && this == rule;

        public override int GetHashCode()
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ RequiredMorph.GetHashCode();
            hash = (hash * 16777619) ^ RejectedMorph.GetHashCode();
            hash = (hash * 16777619) ^ RequiredAttributes.GetHashCode();
            hash = (hash * 16777619) ^ RejectedAttributes.GetHashCode();

            return hash;
        }

        public static bool operator ==(MorphemeRule rule1, MorphemeRule rule2) =>
            rule1.RequiredMorph == rule2.RequiredMorph &&
            rule1.RejectedMorph == rule2.RejectedMorph &&
            rule1.RequiredAttributes == rule2.RequiredAttributes &&
            rule1.RejectedAttributes == rule2.RejectedAttributes;

        public static bool operator !=(MorphemeRule rule1, MorphemeRule rule2) => !(rule1 == rule2);

    }
}
