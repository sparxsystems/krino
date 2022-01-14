using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Verb : PhraseBase, IVerb
    {
        public Verb(BigInteger attributes)
            : base(attributes)
        {
        }
    }
}
