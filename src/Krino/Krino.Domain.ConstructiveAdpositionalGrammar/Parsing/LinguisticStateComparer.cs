using System.Collections.Generic;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Parsing
{
    public class LinguisticStateComparer : IEqualityComparer<LinguisticState>
    {
        public bool Equals(LinguisticState x, LinguisticState y) => x.Id == y.Id;

        public int GetHashCode(LinguisticState obj) => obj.Id.GetHashCode();
    }
}
