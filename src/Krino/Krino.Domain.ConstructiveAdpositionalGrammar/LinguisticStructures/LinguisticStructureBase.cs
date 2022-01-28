using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures
{
    public abstract class LinguisticStructureBase
    {
        private EnumRootBase myEnumRoot;

        public LinguisticStructureBase(EnumRootBase enumRoot, BigInteger attributes)
        {
            myEnumRoot = enumRoot;
            Attributes = attributes;
        }


        public virtual BigInteger Attributes { get; protected set; }

        public string AttributesStr => myEnumRoot.GetFullName(Attributes);
    }
}
