using Krino.Vertical.Utils.Rules;
using System;
using System.Numerics;

namespace Krino.ConstructiveGrammar.LinguisticStructures
{
    public interface IMorphemeBinding : IEquatable<IMorphemeBinding>
    {
        BigInteger AttributesToAdd { get; }

        BigInteger AttributesToRemove { get; }

        IRule<IWord> Rule { get; }
    }
}
