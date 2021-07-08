using System;
using System.Collections.Generic;
using System.Linq;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AdTrees
{
    public class AdTreeFactoryComparer : IEqualityComparer<AdTreeFactory>
    {
        public bool Equals(AdTreeFactory a, AdTreeFactory b) => a.Select(x => x.Pattern).SequenceEqual(b.Select(x => x.Pattern));

        public int GetHashCode(AdTreeFactory obj)
        {
            int hash = 486187739;

            foreach (var subFactory in obj)
            {
                hash = (hash * 16777619) ^ subFactory.Pattern.GetHashCode();
            }

            return hash;
        }
    }
}
