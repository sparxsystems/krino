using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class ObjectOfVerbAttributes : EnumGroupBase
    {
        public ObjectOfVerbAttributes(EnumGroupBase parent) : base(parent)
        {
            Direct = new EnumValue(this);
            Indirect = new EnumValue(this);
        }

        public EnumValue Direct { get; }
        public EnumValue Indirect { get; }
    }
}
