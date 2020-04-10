namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    public static class AttributesExt
    {
        /// <summary>
        /// Converts Attributes enum to the number of valencies.
        /// </summary>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public static int ToValencyCount(this Attributes attributes)
        {
            if (attributes.HasFlag(Attributes.Avalent))
            {
                return 0;
            }

            if (attributes.HasFlag(Attributes.Monovalent))
            {
                return 1;
            }

            if (attributes.HasFlag(Attributes.Bivalent))
            {
                return 2;
            }

            if (attributes.HasFlag(Attributes.Trivalent))
            {
                return 3;
            }

            if (attributes.HasFlag(Attributes.Quadrivalent))
            {
                return 4;
            }

            if (attributes.HasFlag(Attributes.Pentavalent))
            {
                return 5;
            }

            return 0;
        }
    }
}
