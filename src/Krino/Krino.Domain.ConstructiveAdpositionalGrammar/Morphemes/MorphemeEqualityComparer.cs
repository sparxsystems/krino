using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes
{
    /// <summary>
    /// Comparer to check if two morphemes are equal.
    /// </summary>
    public class MorphemeEqualityComparer : IEqualityComparer<Morpheme>
    {
        public bool Equals(Morpheme x, Morpheme y)
        {
            if (x.Morph == y.Morph &&
                x.GrammarCharacter == y.GrammarCharacter &&
                x.Attributes == y.Attributes)
            {
                return true;
            }

            return false;
        }

        public int GetHashCode(Morpheme obj)
        {
            int hash = 486187739;

            hash = (hash * 16777619) ^ obj.Morph.GetHashCode();
            hash = (hash * 16777619) ^ obj.GrammarCharacter.GetHashCode();
            hash = (hash * 16777619) ^ obj.Attributes.GetHashCode();

            return hash;
        }
    }
}
