using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class BoundMorphemeAttributes : EnumGroupBase
    {
        public BoundMorphemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new AffixAttributes(this);
            Suffix = new AffixAttributes(this);
        }

        public AffixAttributes Prefix { get; }
        public AffixAttributes Suffix { get; }
    }
}
