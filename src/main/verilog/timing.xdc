create_clock -name clock -period 4.000 [ get_ports clock ]
set_false_path -from reset -to -clock
