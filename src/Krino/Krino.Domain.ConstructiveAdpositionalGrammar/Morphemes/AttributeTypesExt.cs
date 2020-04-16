namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public static class AttributeTypesExt
    {
        /// <summary>
        /// Converts Attributes enum to the number of valencies.
        /// </summary>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static int ToValencyCount(this AttributeTypes attributes)
        {
            if (attributes.HasFlag(AttributeTypes.Avalent))
            {
                return 0;
            }

            if (attributes.HasFlag(AttributeTypes.Monovalent))
            {
                return 1;
            }

            if (attributes.HasFlag(AttributeTypes.Bivalent))
            {
                return 2;
            }

            if (attributes.HasFlag(AttributeTypes.Trivalent))
            {
                return 3;
            }

            if (attributes.HasFlag(AttributeTypes.Quadrivalent))
            {
                return 4;
            }

            if (attributes.HasFlag(AttributeTypes.Pentavalent))
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
        public static bool IsValencySpecified(this AttributeTypes attributes)
        {
            return (attributes & AttributeTypes.AllValenciesMask) != AttributeTypes.None;
        }
    }
}
