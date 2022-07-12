using Krino.ConstructiveGrammar.LinguisticStructures;
using Krino.Vertical.Utils.Rules;
using Krino.Vertical.Utils.Transformations;
using System;
using System.Numerics;

namespace Krino.ConstructiveGrammar.Morphology
{
    public class AffixBinding : IMorphemeBinding
    {
        public BigInteger AttributesToAdd { get; init; }

        public BigInteger AttributesToRemove { get; init; }

        public IRule<IWord> Rule { get; init; } = RuleMaker.Anything<IWord>();

        public ITransformation<string> TransformWord { get; init; } = Trans.NothingToDo<string>();

        public bool Equals(IMorphemeBinding other)
            => AttributesToAdd == other.AttributesToAdd &&
               AttributesToRemove == other.AttributesToRemove &&
               (Rule == other.Rule || Rule != null && other.Rule != null && Rule.Equals(other.Rule));


        public override bool Equals(object obj) => obj is AffixBinding other && Equals(other);

        public static bool operator ==(AffixBinding a, AffixBinding b) => a.Equals(b);

        public static bool operator !=(AffixBinding a, AffixBinding b) => !(a == b);

        public override int GetHashCode() => HashCode.Combine(AttributesToAdd.GetHashCode(), AttributesToRemove.GetHashCode(), Rule.GetHashCode());
    }
}
