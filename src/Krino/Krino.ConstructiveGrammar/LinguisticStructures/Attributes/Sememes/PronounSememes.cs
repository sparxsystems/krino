﻿using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes
{
    public class PronounSememes : EnumGroupBase
    {
        public PronounSememes(EnumGroupBase parent) : base(parent)
        {
            Person = new PersonSememes(this);
            Gender = new GenderSememes(this);
            Number = new NumberSememes(this);
            Case = new CaseSememes(this);
        }

        public PersonSememes Person { get; }

        public GenderSememes Gender { get; }

        public NumberSememes Number { get; }

        public CaseSememes Case { get; }
    }
}
