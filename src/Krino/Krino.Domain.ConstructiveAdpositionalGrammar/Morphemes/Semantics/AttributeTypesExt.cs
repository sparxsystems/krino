namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Semantics
{
    public static class AttributeTypesExt
    {
        /// <summary>
        /// Converts Attributes enum to the number of valencies.
        /// </summary>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static int ToValencyCount(this VerbAttributeTypes attributes)
        {
            if (attributes.HasFlag(VerbAttributeTypes.Avalent))
            {
                return 0;
            }

            if (attributes.HasFlag(VerbAttributeTypes.Monovalent))
            {
                return 1;
            }

            if (attributes.HasFlag(VerbAttributeTypes.Bivalent))
            {
                return 2;
            }

            if (attributes.HasFlag(VerbAttributeTypes.Trivalent))
            {
                return 3;
            }

            if (attributes.HasFlag(VerbAttributeTypes.Quadrivalent))
            {
                return 4;
            }

            if (attributes.HasFlag(VerbAttributeTypes.Pentavalent))
            {
                return 5;
            }

            return 0;
        }

        /// <summary>
        /// Returns true if a valency is peciefied in Attributes.
        /// </summary>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static bool IsValencySpecified(this VerbAttributeTypes attributes)
        {
            return (attributes & VerbAttributeTypes.AllValenciesMask) != VerbAttributeTypes.None;
        }
    }
}
