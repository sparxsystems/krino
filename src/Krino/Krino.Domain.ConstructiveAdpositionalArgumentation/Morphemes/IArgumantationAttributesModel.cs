using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation.Morphemes
{
    public interface IArgumantationAttributesModel
    {
        bool IsPremise(BigInteger attributes);
        bool IsConclusion(BigInteger attributes);
    }
}
