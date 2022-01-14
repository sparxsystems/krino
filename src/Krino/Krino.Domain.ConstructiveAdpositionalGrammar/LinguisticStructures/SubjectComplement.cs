using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class SubjectComplement : PhraseBase, ISubjectComplement
    {
        public SubjectComplement(BigInteger attributes)
            : base(attributes)
        {
        }
    }
}
