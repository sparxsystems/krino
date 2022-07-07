using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class AuxiliaryVerbAttributes : EnumGroupBase
    {
        public AuxiliaryVerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Primary = new EnumValue(this);
            Modal = new EnumValue(this);
        }

        public EnumValue Primary { get; }

        public EnumValue Modal { get; }
    }
}
