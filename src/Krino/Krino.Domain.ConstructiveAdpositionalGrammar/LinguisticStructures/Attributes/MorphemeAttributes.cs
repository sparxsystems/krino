using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class MorphemeAttributes : EnumGroupBase
    {
        public MorphemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Free = new FreeMorphemeAttributes(this);
            Bound = new BoundMorphemeAttributes(this);
        }

        public FreeMorphemeAttributes Free { get; }
        public BoundMorphemeAttributes Bound { get; }
    }
}
