set mode render
set vmddir [set env(VMDDIR)]
set tachname [glob -directory $vmddir -tails *tachyon*]
set tach "$vmddir/$tachname"
axes location off
stage location off
display showdows on
display ambientocclusion on
mol default material AOShiny

if { $mode == "render" } {
   display resize 1000 1000

   set filelist [glob frame_*.vtf]
   set filecount 0
   foreach file [split $filelist] {incr filecount}
   color Display Background white
   color Axes Labels black
   display depthcue off

   set frame 0
   for {set i 0} {$i < $filecount} {incr i} {
      set txfile($i) [lindex [split $filelist] $i]
   }
   for {set i 0} {$i < $filecount} {incr i} {
      set init [mol new atoms 1]
      set sel  [atomselect $init all]
      $sel set name 0
      $sel set name 1
      $sel delete
      mol delete $init
      mol delete all
      mol load vtf $txfile($i)
      pbc box
      set molID [molinfo top]
      mol delrep 0 $molID
      mol representation VDW 1.0 12.0
      mol addrep $molID
      color change rgb 32 0.1 0.1 0.8
      color Name 0 32
      color change rgb 31 0.8 0.1 0.1
      color Name 1 31
      set filename snap.[format "%04d" $frame].bmp
      set datname snap.[format "%04d" $frame].dat
      render Tachyon $datname
      exec $tach 48 $datname -res 1000 1000 -format BMP -o $filename
      exec rm $datname
      render snapshot $filename
      incr frame
   }
}
