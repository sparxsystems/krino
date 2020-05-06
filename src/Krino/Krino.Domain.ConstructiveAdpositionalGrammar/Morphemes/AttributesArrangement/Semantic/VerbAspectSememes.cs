using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Semantic
{
    /// <summary>
    /// Aspect attributes.
    /// </summary>
    public class VerbAspectSememes : EnumGroupBase
    {
        public VerbAspectSememes(EnumGroupBase parent) : base(parent)
        {
            Simple = new EnumValue(this);
            Continous = new EnumValue(this);
            Perfect = new EnumValue(this);
        }

        public EnumValue Simple { get; }
        public EnumValue Continous { get; }
        public EnumValue Perfect { get; }
    }
}
