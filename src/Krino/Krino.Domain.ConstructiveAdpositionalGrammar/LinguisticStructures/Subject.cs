using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    internal class Subject : PhraseBase, ISubject
    {
        public Subject(EnumRootBase enumRoot, BigInteger attributes)
            : base(enumRoot, attributes)
        {
        }
    }
}
