﻿using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class VerbNumberForm : EnumGroupBase
    {
        public VerbNumberForm(EnumGroupBase parent) : base(parent)
        {
            Singular = new VerbPersonForm(this);
            Plural = new VerbPersonForm(this);
        }

        public VerbPersonForm Singular { get; }
        public VerbPersonForm Plural { get; }
    }
}