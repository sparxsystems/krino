using System;
using System.Collections.Generic;
using System.Numerics;
using System.Text;

namespace Krino.Domain.ConstructiveAdpositionalArgumentation.Morphemes
{
    public interface IArgumantationAttributesModel
    {
        bool IsPremise(BigInteger attributes);
    }
}
