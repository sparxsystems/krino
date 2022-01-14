using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Subject : PhraseBase, ISubject
    {
        public Subject(BigInteger attributes)
            : base(attributes)
        {
        }
    }
}
