using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural
{
    public class ObjectAttributes : EnumGroupBase
    {
        public ObjectAttributes(EnumGroupBase parent) : base(parent)
        {
            Direct = new EnumValue(this);
            Indirect = new EnumValue(this);
        }

        public EnumValue Direct { get; }
        public EnumValue Indirect { get; }
    }
}
