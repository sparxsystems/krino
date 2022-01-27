using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Complement : PhraseBase, IComplement
    {
        public Complement(BigInteger attributes)
            : base(attributes)
        {
        }
    }
}
