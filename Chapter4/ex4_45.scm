#lang sicp

(#%require racket/include)
(include "grammar.scm")

(parse '(the professor lectures to the student
             in the class with the cat))
; 1.
;{sentence
; {simple-noun-phrase {article the} {noun professor}}
; {verb-phrase
;  {verb-phrase
;   {verb-phrase
;    {verb lectures}
;    {prep-phrase
;     {prep to}
;     {simple-noun-phrase {article the} {noun student}}}}
;   {prep-phrase
;    {prep in}
;    {simple-noun-phrase {article the} {noun class}}}}
;  {prep-phrase
;   {prep with}
;   {simple-noun-phrase {article the} {noun cat}}}}}

; The professor lectures to the student. The professor is in
; the class. The professor is with the cat.

; 2.
;{sentence
; {simple-noun-phrase {article the} {noun professor}}
; {verb-phrase
;  {verb-phrase
;   {verb lectures}
;   {prep-phrase
;    {prep to}
;    {simple-noun-phrase {article the} {noun student}}}}
;  {prep-phrase
;   {prep in}
;   {noun-phrase
;    {simple-noun-phrase {article the} {noun class}}
;    {prep-phrase
;     {prep with}
;     {simple-noun-phrase {article the} {noun cat}}}}}}}

; The professor lectures to the student. Inside the class which
; has the cat.

; 3.
;{sentence
; {simple-noun-phrase {article the} {noun professor}}
; {verb-phrase
;  {verb-phrase
;   {verb lectures}
;   {prep-phrase
;    {prep to}
;    {noun-phrase
;     {simple-noun-phrase {article the} {noun student}}
;     {prep-phrase
;      {prep in}
;      {simple-noun-phrase {article the} {noun class}}}}}}
;  {prep-phrase
;   {prep with}
;   {simple-noun-phrase {article the} {noun cat}}}}}

; The professor lectures to the student. The student is in
; the class. The professor is with the cat.

; 4.
;{sentence
; {simple-noun-phrase {article the} {noun professor}}
; {verb-phrase
;  {verb lectures}
;  {prep-phrase
;   {prep to}
;   {noun-phrase
;    {noun-phrase
;     {simple-noun-phrase {article the} {noun student}}
;     {prep-phrase
;      {prep in}
;      {simple-noun-phrase {article the} {noun class}}}}
;    {prep-phrase
;     {prep with}
;     {simple-noun-phrase {article the} {noun cat}}}}}}}

; The professor lectures to the student. The student is in
; the class. The student is with the cat.

; 5.
;{sentence
; {simple-noun-phrase {article the} {noun professor}}
; {verb-phrase
;  {verb lectures}
;  {prep-phrase
;   {prep to}
;   {noun-phrase
;    {simple-noun-phrase {article the} {noun student}}
;    {prep-phrase
;     {prep in}
;     {noun-phrase
;      {simple-noun-phrase {article the} {noun class}}
;      {prep-phrase
;       {prep with}
;       {simple-noun-phrase {article the} {noun cat}}}}}}}}}

; The professor lectures to the student. The student is in
; the class. The class with the cat.