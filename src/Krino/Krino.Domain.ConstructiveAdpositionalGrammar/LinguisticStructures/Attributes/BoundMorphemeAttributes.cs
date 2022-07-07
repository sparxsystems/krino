using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class BoundMorphemeAttributes : EnumGroupBase
    {
        public BoundMorphemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Derivational = new EnumValue(this);
            Inflectional = new EnumValue(this);
            Prefix = new EnumValue(this);
            Suffix = new EnumValue(this);
        }

        public EnumValue Derivational { get; }
        public EnumValue Inflectional { get; }

        public EnumValue Prefix { get; }
        public EnumValue Suffix { get; }
    }
}
